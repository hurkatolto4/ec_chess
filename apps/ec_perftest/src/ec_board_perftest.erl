-module(ec_board_perftest).

-export([start/1]).

-include_lib("ec_chess/include/ec.hrl").
-include_lib("ec_chess/include/ec_perf.hrl").

-define(PERF(C, St, Op),
        timer:tc(fun() -> ec_board:iterate(C, St, Op) end)).

start(Count) ->
    init_board_test(Count).

init_board_test(C) ->
    St = ec_board:start_board(),
    %% simple pawn movements
    {T1, _} = ?PERF(C, op_cond, [St, {{2,2},{2,4}}, true]),
    ?LOG_PERF("Init state, pawn move", C, T1),
    {T2, _} = ?PERF(C, op_cond, [St, {{2,2},{2,3}}, true]),
    ?LOG_PERF("Init state, pawn move simple", C, T2),
    {T3, _} = ?PERF(C, op_cond, [St, {{2,1},{1,3}}, true]),
    ?LOG_PERF("Init state, pawn move simple", C, T3).

