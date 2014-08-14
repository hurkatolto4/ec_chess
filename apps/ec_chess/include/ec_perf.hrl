-ifndef(__EC_PERF__).
-define(__EC_PERF__, true).

-export([iterate/3]).

-spec iterate(C :: integer(), Function :: atom(), Args :: list()) ->
    Result :: integer().
iterate(0, _Function, _Args) ->
    ok;
iterate(C, Function, Args) ->
    apply(?MODULE, Function, Args),
    iterate(C - 1, Function, Args).

display_performance(Str, Count, Time) ->
    Perf = trunc(Count * (1000000 / Time)),
    io:format("~40s     - ~p~n", [Str, Perf]).

-endif.
