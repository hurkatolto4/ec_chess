%%------------------------------------------------------------------------------
%% eunit tests for ec_board
%%------------------------------------------------------------------------------
-module(ec_board_test).

-include("ec.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(octest(Exp, State, Op),
        ?assertMatch(Exp, ec_board:op_cond(State, Op, true))
    ).

-define(mtest(Exp, State),
        ?assertEqual(Exp, ec_board:is_check_mate(State))
    ).

init_test() ->
    St = ec_board:start_board(),

    %% Pawn simple moves
    ?octest(ok, St, {{2,2},{2,3}}),
    ?octest(ok, St, {{2,2},{2,4}}),
    ?octest(ok, St, {{3,2},{3,4}}),
    % Negative
    ?octest({error, {?ECE_NOT_VALID, _}},
            St, {{3,2},{3,5}}),  %% can't move 3
    ?octest({error, {?ECE_NOT_VALID, _}},
             St, {{3,2},{4,3}}),

    % Knight
    ?octest(ok, St, {{2,1},{1,3}}),
    ?octest(ok, St, {{2,1},{3,3}}),
    ?octest(ok, St, {{7,1},{6,3}}),
    ?octest(ok, St, {{7,1},{8,3}}),
    % Negative
    ?octest({error, {?ECE_SAME_COLOR, _}}, St, {{2,1},{4,2}}),
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{2,1},{8,3}}).

%% State after:
%% 1: e4, e5
%% 2: f3, c6
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
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?WN, ?OO, ?OO,
                     ?WP, ?WP, ?WP, ?WP, ?OO, ?WP, ?WP, ?WP,
                     ?WR, ?WN, ?WB, ?WQ, ?WK, ?WB, ?OO, ?WR}
        },
    %% White bishop
    ?octest(ok, St, {{6,1},{2,5}}),
    ?octest(ok, St, {{6,1},{3,4}}),
    ?octest(ok, St, {{6,1},{5,2}}),
    ?octest({error, {?ECE_SAME_FIELDS, _}}, St, {{6,1}, {6,1}}),
    ?octest({error, {?ECE_SAME_COLOR, _}}, St, {{6,1}, {5,1}}),
    ?octest({error, {?ECE_SAME_COLOR, _}}, St, {{3,1}, {2,2}}),
    ?octest({error, {?ECE_PIECE_ON_PATH, _}}, St, {{3,1}, {5,3}}),
    ?octest({error, {?ECE_PIECE_ON_PATH, _}}, St, {{6,1}, {8,3}}),
    ?octest({error, {?ECE_NOT_YOUR_TURN, _}}, St, {{6,8}, {5,7}}),
    %% white quuen
    ?octest(ok, St, {{4,1},{5,2}}),
    ?octest({error, {?ECE_PIECE_ON_PATH, _}}, St, {{4,1},{4,3}}),
    ?octest({error, {?ECE_PIECE_ON_PATH, _}}, St, {{4,1},{7,4}}).

board_2_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?BQ, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WQ, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO, ?OO}
        },
    %% white queen
    ?octest({error, {?ECE_NOT_YOUR_TURN, _}}, St, {{4,2},{4,3}}),
    %% black queen
    ?octest(ok, St, {{4,7},{4,3}}),
    ?octest(ok, St, {{4,7},{4,2}}),
    ?octest({error, {?ECE_CHECK_AFTER_MOVE, _}}, St, {{4,7}, {5,7}}),
    ?octest({error, {?ECE_CANT_TAKE_KING, _}}, St, {{4,7}, {4,1}}).

board_3_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?WR, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WR, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WQ, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO, ?OO}
        },
    %% Black can move out from check only by taking the rook on e8
    ?octest(ok, St, {{4,8},{5,8}}),
    %% Can't take the other rook
    ?octest({error, {?ECE_CHECK_AFTER_MOVE, _}}, St, {{4,8}, {4,7}}).

board_4_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO}
        },
    ?octest(ok, St, {{4,8},{3,8}}),
    ?octest(ok, St, {{4,8},{5,8}}),
    ?octest({error, {?ECE_CHECK_AFTER_MOVE, _}}, St, {{4,8},{4,7}}).

%% Test an-passant
board_5_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?WHITE,
            last_move = {{4,7},{4,5}},
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?BP, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?BP, ?WP, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO, ?OO}
        },
    ?octest(ok, St, {{5,5},{5,6}}),
    ?octest(ok, St, {{5,5},{6,6}}),
    %% en-passan trick
    ?octest(ok, St, {{5,5},{4,6}}).

%% En-passan not possible if the last move was not the required one.
board_6_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?WHITE,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?BP, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?BP, ?WP, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO, ?OO}
        },
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,5},{4,6}}).

%% castle related test
board_7_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = true,
            to_move = ?WHITE,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?BB, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?WR, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?WR}
        },
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,1},{7,1}}).

%% castle related test
board_8_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = true,
            to_move = ?WHITE,
            last_move = undefined,
            board = {?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?BB, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?WR, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?WR}
        },
    ?octest(ok, St, {{5,1},{3,1}}),
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,1},{7,1}}).

%% castle related test
%% test the same on the black side
board_9_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?BR, ?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?BR,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?WB, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO}
        },
    ?octest(ok, St, {{5,8},{7,8}}),
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,8},{3,8}}).

%% castle related test
%% test the same on the black side
board_10_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?BR, ?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?WB, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO}
        },
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,8},{7,8}}),
    ?octest({error, {?ECE_NOT_VALID, _}}, St, {{5,8},{3,8}}).

mate_1_test() ->
    St = #board_state{
            wk_castled = false,
            bk_castled = false,
            to_move = ?BLACK,
            last_move = undefined,
            board = {?WR, ?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO,
                     ?OO, ?WR, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?WK, ?OO, ?OO, ?OO}
        },
    ?mtest(true, St).

mate_2_test() ->
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
                     ?OO, ?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?BR, ?OO, ?WK, ?OO, ?OO, ?OO}
        },
    ?mtest(true, St).

mate_3_test() ->
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
                     ?OO, ?OO, ?OO, ?OO, ?BK, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
                     ?OO, ?OO, ?BR, ?OO, ?OO, ?WK, ?OO, ?OO}
        },
    ?mtest(false, St).

%% white is not in check currently but he can't move (stalemate)
mate_4_test() ->
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
    ?mtest(false, St).

-endif.
