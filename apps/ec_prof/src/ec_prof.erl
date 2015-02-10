-module(ec_prof).

-export([fprof/2]).

-record(state, {totalCount=0, totalAcc=0, totalOwn=0, ids=0, seen, hot, links}).

-record(function, {id, count=0, acc=0, own=0}).

-define(THRESHOLD, 0.001).
-define(BASE_SIZE, 6).

-define(TMP_FILE, "/tmp/fprof.tmp").
-define(DEF_FPROF_OPTS, [verbose, {procs, all}]).

-define(W2F(Fmt, Args), file:write_file(?TMP_FILE, lists:flatten(io_lib:format(Fmt, Args)), [append])).

nameBin({M,F,A}) ->
    iolist_to_binary([atom_to_list(M), ":", atom_to_list(F), "/", integer_to_list(A)]);
nameBin(N) -> iolist_to_binary(atom_to_list(N)).

maybeLinkSeen(SomeFun, Nid, D) ->
    case dict:is_key(SomeFun, D) of
        true ->
            {Nid, D};
        _ ->
            {Nid+1, dict:store(SomeFun, Nid+1, D)}
    end.

recordOne(Callers, {Name, Count, Acc, Own}, Callees, State) ->
    {NextId, NextSeen} = lists:foldl(fun({SomeFun,_,_,_}, {Nid, D}) ->
                                            maybeLinkSeen(SomeFun, Nid, D);
                                        (SomeFun, {Nid, D}) ->
                                            maybeLinkSeen(SomeFun, Nid, D)
                                     end,
                                     {State#state.ids, State#state.seen},
                                     [Name] ++ Callers ++ Callees),

    Newd = dict:update(Name,
                       fun(#function{count=CCount, acc=CAcc, own=COwn}) ->
                            #function{count=CCount + Count,
                                      acc=CAcc + Acc, own=COwn + Own}
                       end,
                       #function{id=State#state.ids, count=Count, acc=Acc, own=Own},
                       State#state.hot),
    InLinks = lists:foldl(fun({Caller, CCount, CAcc, COwn}, D) ->
                                dict:update({Caller, Name},
                          fun({ICount, IAcc, IOwn}) ->
                                {ICount + CCount,
                                IAcc + CAcc,
                                IOwn + COwn}
                          end,
                          {CCount, CAcc, COwn},
                          D)
                         end, State#state.links, Callers),

    OutLinks = lists:foldl(fun({Callee, CCount, CAcc, COwn}, D) ->
                                dict:update({Name, Callee},
                           fun({ICount, IAcc, IOwn}) ->
                                {ICount + CCount,
                                 IAcc + CAcc,
                                 IOwn + COwn}
                           end,
                           {CCount, CAcc, COwn},
                           D)
                    end, InLinks, Callees),

    State#state{hot=Newd, ids=NextId, seen=NextSeen, links=OutLinks}.

displayFunction(Callers, {Name, Count, Acc, Own}=Self, Callees, State) ->
    Marker = case (Own / State#state.totalOwn) > ?THRESHOLD of
                true -> "(*)";
                _ -> ""
             end,
    ?W2F("// function~s: ~s (~p, ~p, ~p)~n", [Marker, nameBin(Name), Count, Acc, Own]),
    case (Own / State#state.totalOwn) > ?THRESHOLD of
        true -> recordOne(Callers, Self, Callees, State);
        _ -> State
    end.

grok([], State) -> State;
grok([{analysis_options, Options}|Tl], State) ->
    ?W2F("// Enabled options: ~p~n", [Options]),
    grok(Tl, State);
grok([[{totals, TotalCount, TotalAcc, TotalOwn}]|Tl], OldState) ->
    ?W2F("// Totals: Count: ~p, Acc: ~p, Own: ~p~n",
    [TotalCount, TotalAcc, TotalOwn]),
    grok(Tl, OldState#state{totalCount=TotalCount,
                            totalAcc=TotalAcc,
                            totalOwn=TotalOwn});
grok([[{ _Pid, _Count, _Acc, _Own},
        { spawned_by, _PPid},
        { spawned_as, { Module, Function, Args}},
        { initial_calls, _InitialCalls }]|Tl], State) ->
    ?W2F("// Got a process: ~p:~p/~p~n", [Module, Function, length(Args)]),
    grok(Tl, State);
grok([{Callers, Me, Callees}|Tl], State) ->
    grok(Tl, displayFunction(Callers, Me, Callees, State));
grok([Hd|Tl], State) ->
    ?W2F(" /* junk unknown:~n~p~n */~n", [Hd]),
    grok(Tl, State).


define_nodes(Name, Id, State, MinPercent, MaxPercent) ->
    case dict:find(Name, State#state.hot) of
        {ok, Func} ->
            Percent = (Func#function.own * 100) / State#state.totalOwn,
            Extra = case (range_map(Percent, MinPercent, MaxPercent, 0, 100) > 50) of
                        true -> ",color=red";
                        _ -> ""
                    end,
            ?W2F(" N~p [label=\"~s\\n(~p calls, ~.2f%)\",fontsize=~.2f~s];~n",
            [Id, nameBin(Name), Func#function.count, Percent,
             range_map(Percent, MinPercent, MaxPercent, 8, 48),
             Extra]);
        error ->
            Shape = case Name of
                        {_M, _F, _A} -> "oval";
                        _ -> "pentagon"
                    end,
            ?W2F(" N~p [label=\"~s\",fontsize=8,shape=~p]; // not hot~n",
            [Id, nameBin(Name), Shape])
    end,
    State.

get_id({Func, _, _, _}, D) ->
    dict:fetch(Func, D);
get_id(Func, D) ->
    dict:fetch(Func, D).

range_map(Val, FromLow, FromHigh, ToLow, ToHigh) ->
    (Val - FromLow) * (ToHigh - ToLow) / (FromHigh - FromLow) + ToLow.

draw_links(State, LineMax) ->
    dict:fold(fun({From, To}, {_CCount, _CAcc, COwn}, _) ->
                FromId = get_id(From, State#state.seen),
                ToId = get_id(To, State#state.seen),
                LineWidth = range_map(COwn, 0, LineMax, 0.5, 7),
                Extra = case (LineWidth >= 4) of
                            true -> " color=red,";
                            _ -> ""
                        end,
                ?W2F(" N~p -> N~p [label=~p,~s style=\"setlinewidth(~.2f)\"];~n",
                          [FromId, ToId, COwn, Extra, LineWidth])
            end, ok, State#state.links),
    State.

find_range(State) ->
    Total = State#state.totalOwn,
    dict:fold(fun(_, V, {L,H}) ->
                    X = (V#function.own * 100) / Total,
                    {min(L, X), max(H, X)}
              end,
              {100, 0},
              State#state.hot).

find_line_max(D) ->
    lists:max(lists:flatten(
        lists:map(fun({_, {_, _, C}}) -> C end,
            dict:to_list(D)))).

fprof(Fun, FileName) ->
    fprof(Fun, FileName, ?DEF_FPROF_OPTS).

fprof(Fun, FileName, Opts) ->
    InFile = FileName ++ ".trc",
    OutFile = FileName ++ ".dot",
    [file:delete(F) || F <- [InFile, OutFile, ?TMP_FILE]],
    fprof:trace([start, {file, "./my.trace"} | Opts]),
    Fun(),
    fprof:trace([stop]),
    fprof:profile({file, "./my.trace"}),
    fprof:analyse([totals, {dest, InFile}]),
    create_png(InFile, OutFile, FileName),
    [file:delete(F) || F <- [InFile, OutFile, ?TMP_FILE, "./my.trace"]].

create_png(InFile, OutFile, FileName) ->
    ?W2F("digraph \"analysis\" {~n", []),
    ?W2F(" node [shape=box];~n", []),
    {ok, Terms} = file:consult(InFile),
    State = grok(Terms, #state{hot=dict:new(), seen=dict:new(), links=dict:new()}),
    {LowestPercent, HighestPercent} = find_range(State),
    State2 = dict:fold(fun(K, V, S) ->
                            define_nodes(K, V, S, LowestPercent, HighestPercent)
                       end,
                       State, State#state.seen),
    LineMax = find_line_max(State2#state.links),
    ?W2F(" // LineMax=~p~n", [LineMax]),
    draw_links(State2, LineMax),
    ?W2F("}~n", []),
    file:copy(?TMP_FILE, OutFile),
    render_image(OutFile, FileName).

render_image(File, FileName) ->
    Cmd = lists:flatten(io_lib:format("dot -Tpng ~s -o ~s.png", [File, FileName])),
    io:format("~p ~p Cmd: '~p' ~n", [?MODULE, ?LINE, Cmd]),
    Res = os:cmd(Cmd),
    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]).
