-module(ec_board_perftest).

-export([start/1]).

-include_lib("ec_chess/include/ec.hrl").

start(Count) ->
    init_board_test(Count).

init_board_test(C) ->
    St = ec_board:start_board(),
    %% simple pawn movements
    {T1, _} = timer:tc(fun() ->ec_board:perftest(C, St, {{2,2},{2,4}}) end),
    display_performance("Init state, pawn move", C, T1),
    {T2, _} = timer:tc(fun() ->ec_board:perftest(C, St, {{2,2},{2,3}}) end),
    display_performance("Init state, pawn move simple", C, T2),
    {T3, _} = timer:tc(fun() ->ec_board:perftest(C, St, {{2,1},{1,3}}) end),
    display_performance("Init state, pawn move simple", C, T3).

display_performance(Str, Count, Time) ->
    Perf = trunc(Count * (1000000 / Time)),
    io:format("~40s     - ~p~n", [Str, Perf]).


