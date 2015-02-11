-module(ec_board_perftest).

-export([start/1]).

-include_lib("ec_chess/include/ec.hrl").
-include_lib("ec_chess/include/ec_perf.hrl").

-define(PERF(C, St, Op),
        timer:tc(fun() -> ec_board:iterate(C, St, Op) end)).

start(Count) ->
    init_board_test(Count),
    mate_1_test(Count).

init_board_test(C) ->
    St = ec_board:start_board(),
    %% simple pawn movements
    {T1, _} = ?PERF(C, op_cond, [St, {{2,2},{2,4}}, true]),
    ?LOG_PERF("Init state, pawn move", C, T1),
    {T2, _} = ?PERF(C, op_cond, [St, {{2,2},{2,3}}, true]),
    ?LOG_PERF("Init state, pawn move simple", C, T2),
    {T3, _} = ?PERF(C, op_cond, [St, {{2,1},{1,3}}, true]),
    ?LOG_PERF("Init state, pawn move simple", C, T3).

mate_1_test(C) ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?WHITE,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?BR, ?BK, ?BR, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO}
        },
    {T1, _} = ?PERF(C, is_check_mate, [St]),
    ?LOG_PERF("Stale mate with two rooks - mate test:", C, T1).
