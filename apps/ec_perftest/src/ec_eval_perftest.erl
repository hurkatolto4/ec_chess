-module(ec_eval_perftest).

-export([start/1]).

-include_lib("ec_chess/include/ec.hrl").
-include_lib("ec_chess/include/ec_perf.hrl").

-define(PERF(C, Func, Args),
        timer:tc(fun() -> ec_eval:iterate(C, Func, Args) end)).

start(Count) ->
    init_board_test(Count).

init_board_test(C) ->
    St = ec_board:start_board(),
    %% simple pawn movements
    {T1, _} = ?PERF(C, eval, [St]),
    display_performance("Init state eval test", C, T1).
