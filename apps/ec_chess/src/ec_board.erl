-module(ec_board).

-export([
    op_cond/2,
    init_board/0,
    start_board/0,
    perftest/3
]).


-compile(inline).
-compile({inline_size, 200}).

-include("ec.hrl").
%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------


-spec perftest(C :: integer(), State :: #board_state{}, Op :: operator()) ->
    Result :: integer().
perftest(0, _State, Op) ->
    ok;
perftest(C, State, Op) ->
    ec_board:op_cond(State, Op),
    perftest(C - 1, State, Op).

-spec op_cond(State, Op) -> boolean() when
    State :: #board_state{},
    Op :: operator().
op_cond(#board_state{} = State, Op) ->
    case check_not_equal(Op) andalso check_field_limits(Op) of
        true ->
            op_cond2(State, Op);
        false ->
            false
    end.

-spec start_board() -> #board_state{}.
start_board() ->
    #board_state{
       wk_castled = false,
       bk_castled = false,
       to_move = ?WHITE,
       last_move = undefined,
       board = init_board()
    }.

-spec init_board() -> board().
init_board() ->
    {?BR, ?BN, ?BB, ?BQ, ?BK, ?BB, ?BN, ?BR,
     ?BP, ?BP, ?BP, ?BP, ?BP, ?BP, ?BP, ?BP,
     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
     ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO, ?OO,
     ?WP, ?WP, ?WP, ?WP, ?WP, ?WP, ?WP, ?WP,
     ?WR, ?WN, ?WB, ?WQ, ?WK, ?WB, ?WN, ?WR}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec op_cond2(State, Op) -> boolean() when
    State :: #board_state{},
    Op :: operator().
op_cond2(#board_state{board = Board, to_move = ToMove} = State, Op) ->
    case {get_piece(Board, element(1, Op)), get_piece(Board, element(2, Op))} of
        {?OO, _} ->
            false;
        {FromPiece, ToPiece} ->
            case {color(FromPiece), color(ToPiece)} of
                {?WHITE, ?WHITE} ->
                    false;
                {?BLACK, ?BLACK} ->
                    false;
                {Col, _} when Col =/= ToMove ->
                    false;
                {Col, _} ->
                    op_cond(FromPiece, State, Op) andalso
                        not check_chess_after_op(State, Op, Col)
            end
    end.

-spec op_cond(FromPiece, State, Op) -> Result when
      State :: #board_state{},
      Op :: operator(),
      FromPiece :: integer(),
      Result :: boolean().
%% White pawn
op_cond(?WP, State, Op) ->
    Col = color(?WP),
    is_pawn_simple_move(State, Op, Col) orelse is_pawn_take(State, Op, Col)
        orelse is_en_passant(State, Op, Col);
%% White rook
op_cond(?WR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White knight
op_cond(?WN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
       (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1);
%% White bishop
op_cond(?WB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White queen
op_cond(?WQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White king
op_cond(?WK, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0);
%% Black pawn
op_cond(?BP, State, Op) ->
    Col = color(?BP),
    is_pawn_simple_move(State, Op, Col) orelse is_pawn_take(State, Op, Col)
        orelse is_en_passant(State, Op, Col);
%% Black rook
op_cond(?BR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black knight
op_cond(?BN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
       (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1);
%% Black bishop
op_cond(?BB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black queen
op_cond(?BQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black king
op_cond(?BK, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0).

-spec get_piece(Board :: board(), Pos :: position() | integer()) -> integer().
get_piece(Board, {X, Y}) ->
    element(?POS(X, Y), Board);
get_piece(Board, Pos) ->
    element(Pos, Board).

%%------------------------------------------------------------------------------
%% @doc Returns true only if all the fields not counting the start and stop
%%      positions are empty between the fwo coordinates
%%------------------------------------------------------------------------------
-spec check_empty_fields_between(Board, Fx, Fy, Tx, Ty) -> Result when
      Board :: board(),
      Fx :: integer(),
      Fy :: integer(),
      Tx :: integer(),
      Ty :: integer(),
      Result :: boolean().
check_empty_fields_between(Board, Fx, Fy, Tx, Ty) ->
    Dx = get_direction(Fx, Tx),
    Dy = get_direction(Fy, Ty),
    check_empty_fields(Board, Fx + Dx, Fy + Dy, Dx, Dy, Tx, Ty).

check_empty_fields(_Board, CurX, CurY, _DX, _DY, TX, TY) when
      CurX =:= TX, CurY =:= TY ->
    true;
check_empty_fields(Board, CurX, CurY, DX, DY, TX, TY) ->
    case get_piece(Board, {CurX, CurY}) of
        ?OO ->
            check_empty_fields(Board, CurX + DX, CurY + DY, DX, DY, TX, TY);
        _ ->
            false
    end.

get_direction(F, T) when T > F -> 1;
get_direction(F, T) when T < F -> -1;
get_direction(_, _) -> 0.

color(?OO) -> ?EMPTY;
color(?WP) -> ?WHITE;
color(?WR) -> ?WHITE;
color(?WN) -> ?WHITE;
color(?WB) -> ?WHITE;
color(?WQ) -> ?WHITE;
color(?WK) -> ?WHITE;
color(?BP) -> ?BLACK;
color(?BR) -> ?BLACK;
color(?BN) -> ?BLACK;
color(?BB) -> ?BLACK;
color(?BQ) -> ?BLACK;
color(?BK) -> ?BLACK.

-spec diff(number(), number()) -> number().
diff(X, Y) when X > Y -> X - Y;
diff(X, Y) -> Y - X.

-spec check_not_equal(Op :: operator()) -> boolean().
check_not_equal({{Fx, Fy}, {Fx, Fy}} = _Op) -> false;
check_not_equal(_Op) -> true.

-spec check_field_limits(Op :: operator()) ->
    boolean().
check_field_limits({{Fx, Fy}, {Tx, Ty}} = _Op) ->
    between(Fx, 1, 8) andalso between(Fy, 1, 8) andalso between(Tx, 1, 8)
        andalso between(Ty, 1, 8).

between(V, Min, Max) when V >= Min, V =< Max -> true;
between(_V, _Min, _Max) -> false.

%% Returns true if after applying the operator the player who had to move is in
%% chess
-spec check_chess_after_op(State, Op, Color) -> Result when
    State :: #board_state{},
    Op :: operator(),
    Color :: color(),
    Result :: boolean().
check_chess_after_op(State, _Op, Col) ->
    King = case Col of
               ?WHITE -> ?WK;
               ?BLACK -> ?BK
           end,
    KingPos = find_first_piece(State#board_state.board, King),
    false.

find_first_piece(Board, ChessPiece) ->
    find_first_piece(1, Board, ChessPiece).

find_first_piece(65, _Board, _Cp) ->
    throw({error, ?ERR_NO_SUCH_PIECE});
find_first_piece(Pos, Board, Cp) ->
    case get_piece(Board, Pos) of
        Cp -> ?UN_POS(Pos);
        _ -> find_first_piece(Pos + 1, Board, Cp)
    end.

-spec is_pawn_simple_move(State, Op, Col) -> Result when
      State :: #board_state{},
      Op :: operator(),
      Col :: color(),
      Result :: boolean().
is_pawn_simple_move(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?WHITE) when Fx =:= Tx ->
    B = State#board_state.board,
    Result = (Ty - Fy =:= 1 andalso color(get_piece(B, {Tx, Ty})) =:= ?EMPTY) orelse
        (Ty - Fy =:= 2 andalso Fy =:= 2 andalso
            color(get_piece(B, {Tx, Ty})) =:= ?EMPTY andalso
            color(get_piece(B, {Tx, Ty - 1})) =:= ?EMPTY),
    Result;
is_pawn_simple_move(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?BLACK) when Fx =:= Tx ->
    B = State#board_state.board,
    (Ty - Fy =:= -1 andalso color(get_piece(B, {Tx, Ty})) =:= ?EMPTY) orelse
        (Ty - Fy =:= -2 andalso Fy =:= 7 andalso
            color(get_piece(B, {Tx, Ty})) =:= ?EMPTY andalso
            color(get_piece(B, {Tx, Ty + 1})) =:= ?EMPTY);
is_pawn_simple_move(_State, _Op, _Col) ->
    false.

%% True when the pawn takes an other chess piece of the opposite colour
-spec is_pawn_take(State, Op, Col) -> Result when
      State :: #board_state{},
      Op :: operator(),
      Col :: color(),
      Result :: boolean().
is_pawn_take(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?WHITE)
  when abs(Fx - Tx) =:= 1, Ty - Fy =:= 1 ->
    color(get_piece(State#board_state.board, {Tx, Ty})) =:= ?BLACK;
is_pawn_take(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?BLACK)
  when abs(Fx - Tx) =:= 1, Ty - Fy =:= -1 ->
    color(get_piece(State#board_state.board, {Tx, Ty})) =:= ?WHITE;
is_pawn_take(_State, _Op, _Col) ->
    false.


%% TODO
%% True when it is an en passant move
-spec is_en_passant(State, Op, Col) -> Result when
      State :: #board_state{},
      Op :: operator(),
      Col :: color(),
      Result :: boolean().
is_en_passant(_State, _Op, _Col) ->
    false.

