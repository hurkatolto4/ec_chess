%%------------------------------------------------------------------------------
%% eunit tests for ec_board
%%------------------------------------------------------------------------------
-module(ec_board_test).

-include("ec.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(octest(Exp, State, Op),
        ?assertEqual(Exp, ec_board:op_cond(State, Op, true))
    ).

init_test() ->
    St = ec_board:start_board(),

    %% Pawn simple moves
    ?octest(ok, St, {{2,2},{2,3}}),
    ?octest(ok, St, {{2,2},{2,4}}),
    ?octest(ok, St, {{3,2},{3,4}}),
    % Negative
    ?octest({error, _}, St, {{3,2},{3,5}}),  %% can't move 3
    ?octest({error, _}, St, {{3,2},{4,3}}),

    % Knight
    ?octest(ok,, St, {{2,1},{1,3}}),
    ?octest(ok, St, {{2,1},{3,3}}),
    ?octest(ok, St, {{7,1},{6,3}}),
    ?octest(ok, St, {{7,1},{8,3}}),
    % Negative
    ?octest({error, _}, St, {{2,1},{4,2}}),
    ?octest({error, _}, St, {{2,1},{8,3}}).

board_1_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?WHITE,
            last_move = undefined,
            board = {?BR, ?OO, ?BB, ?BQ, ?BK, ?BB, ?BN, ?BR,
                     ?BP, ?BP, ?BP, ?BP, ?OO, ?BP, ?BP, ?BP,
                     ?OO, ?OO, ?BN, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?BP, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?WP, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?WP, ?OO, ?OO, ?WN, ?OO, ?OO,
                     ?WP, ?WP, ?OO, ?WP, ?OO, ?WP, ?WP, ?WP,
                     ?WR, ?WN, ?WB, ?WQ, ?WK, ?WB, ?OO, ?WR}
        }.
%    ?octest(true, St, {{4,1},{1,4}}).


-endif.
