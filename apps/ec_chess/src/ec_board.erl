-module(ec_board).

-compile([native, inline, {inline_size, 300}]).

-export([
    op_cond/3,
    init_board/0,
    start_board/0
]).

-include("ec.hrl").
-include("ec_perf.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec op_cond(State, Op, CheckAfter) -> boolean() when
    State :: #board_state{},
    Op :: operator(),
    CheckAfter :: boolean().
op_cond(#board_state{} = State, Op, CheckAfter) ->
    case check_not_equal(Op) andalso check_field_limits(Op) of
        true ->
            op_cond2(State, Op, CheckAfter);
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

-spec op_cond2(State, Op, CheckAfter) -> boolean() when
    State :: #board_state{},
    Op :: operator(),
    CheckAfter :: boolean().
op_cond2(#board_state{board = Board, to_move = ToMove} = State, Op,
         CheckAfter) ->
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
                    op_cond3(FromPiece, State, Op) andalso
                        (not CheckAfter or
                             not check_chess_after_op(State, Op, Col))
            end
    end.

-spec op_cond3(FromPiece, State, Op) -> Result when
      State :: #board_state{},
      Op :: operator(),
      FromPiece :: integer(),
      Result :: boolean().
%% White pawn
op_cond3(?WP, State, Op) ->
    Col = color(?WP),
    is_pawn_simple_move(State, Op, Col) orelse is_pawn_take(State, Op, Col)
        orelse is_en_passant(State, Op, Col);
%% White rook
op_cond3(?WR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White knight
op_cond3(?WN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
       (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1);
%% White bishop
op_cond3(?WB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White queen
op_cond3(?WQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% White king
op_cond3(?WK, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0);
%% Black pawn
op_cond3(?BP, State, Op) ->
    Col = color(?BP),
    is_pawn_simple_move(State, Op, Col) orelse is_pawn_take(State, Op, Col)
        orelse is_en_passant(State, Op, Col);
%% Black rook
op_cond3(?BR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black knight
op_cond3(?BN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
       (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1);
%% Black bishop
op_cond3(?BB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black queen
op_cond3(?BQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            false
    end;
%% Black king
op_cond3(?BK, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0).

-spec get_piece(Board :: board(), Pos :: position() | integer()) -> integer().
get_piece(Board, {X, Y}) ->
    Pos = ?POS(X, Y),
    element(Pos, Board);
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
check_chess_after_op(State, Op, Col) ->
    NewState = apply_op(State, Op),
    King = case Col of
               ?WHITE -> ?WK;
               ?BLACK -> ?BK
           end,
    KingPos = find_first_piece(NewState#board_state.board, King),
    is_in_check(1, NewState, KingPos, ?NEG_COL(Col)).

is_in_check(65, _State, _KingPos, _OppCol) ->
    false;
is_in_check(C, State, KingPos, OppCol) ->
    FromPos = ?UN_POS(C),
    Col = color(get_piece(State#board_state.board, FromPos)),
    case Col =:= OppCol of
        true ->
            Op = {FromPos, KingPos},
            case op_cond(State, Op, _CheckAfter = false) of
                true -> true;
                false -> is_in_check(C + 1, State, KingPos, OppCol)
            end;
        false ->
            is_in_check(C + 1, State, KingPos, OppCol)
    end.

-spec apply_op(State, Op) -> NewState when
      State :: #board_state{},
      Op :: operator(),
      NewState :: #board_state{}.
apply_op(State, {{Fx, Fy}, {_Tx, _Ty}} = Op) ->
    Piece = get_piece(State#board_state.board, {Fx, Fy}),
    case get_castle_type(Piece, Op) of
        ?CASTLE_WHITE_SHORT -> apply_castle_white_short(State, Op);
        ?CASTLE_WHITE_LONG  -> apply_castle_white_long(State, Op);
        ?CASTLE_BLACK_SHORT -> apply_castle_black_short(State, Op);
        ?CASTLE_BLACK_LONG  -> apply_castle_black_long(State, Op);
        ?CASTLE_NOT         -> apply_simple(State, Op)
    end.

-spec get_castle_type(Piece, Op) -> Result when
    Piece :: chess_piece(),
    Op :: operator(),
    Result :: castle_t().
get_castle_type(?WK, {{5,1},{7,1}}) -> ?CASTLE_WHITE_SHORT;
get_castle_type(?WK, {{5,1},{3,1}}) -> ?CASTLE_WHITE_LONG;
get_castle_type(?BK, {{5,8},{7,8}}) -> ?CASTLE_BLACK_SHORT;
get_castle_type(?BK, {{5,8},{3,8}}) -> ?CASTLE_BLACK_LONG;
get_castle_type(_Piece, _Op) ->        ?CASTLE_NOT.

apply_simple(State, {{Fx, Fy}, {Tx, Ty}} = Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Piece = get_piece(State#board_state.board, {Fx, Fy}),
    Nb1 = set_field(Board, Fx, Fy, ?OO),
    Nb2 = set_field(Nb1, Tx, Ty, Piece),
    State#board_state{
      to_move = ?NEG_COL(ToMove),
      last_move = Op,
      board = Nb2
    }.

-spec apply_castle_white_long(State :: #board_state{}, Op :: operator()) ->
    #board_state{}.
apply_castle_white_long(State, Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Nb1 = set_field(Board, 5, 1, ?OO),
    Nb2 = set_field(Nb1, 4, 1, ?WR),
    Nb3 = set_field(Nb2, 3, 1, ?WK),
    Nb4 = set_field(Nb3, 1, 1, ?OO),
    State#board_state{
      wk_castled = true,
      to_move = ?NEG_COL(ToMove),
      last_move = Op,
      board = Nb4
    }.

-spec apply_castle_white_short(State :: #board_state{}, Op :: operator()) ->
    #board_state{}.
apply_castle_white_short(State, Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Nb1 = set_field(Board, 5, 1, ?OO),
    Nb2 = set_field(Nb1, 6, 1, ?WR),
    Nb3 = set_field(Nb2, 7, 1, ?WK),
    Nb4 = set_field(Nb3, 8, 1, ?OO),
    State#board_state{
      wk_castled = true,
      to_move = ?NEG_COL(ToMove),
      last_move = Op,
      board = Nb4
    }.

-spec apply_castle_black_long(State :: #board_state{}, Op :: operator()) ->
    #board_state{}.
apply_castle_black_long(State, Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Nb1 = set_field(Board, 5, 8, ?OO),
    Nb2 = set_field(Nb1, 4, 8, ?BR),
    Nb3 = set_field(Nb2, 3, 8, ?BK),
    Nb4 = set_field(Nb3, 1, 8, ?OO),
    State#board_state{
      bk_castled = true,
      to_move = ?NEG_COL(ToMove),
      last_move = Op,
      board = Nb4
    }.

-spec apply_castle_black_short(State :: #board_state{}, Op :: operator()) ->
    #board_state{}.
apply_castle_black_short(State, Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Nb1 = set_field(Board, 5, 8, ?OO),
    Nb2 = set_field(Nb1, 6, 8, ?BR),
    Nb3 = set_field(Nb2, 7, 8, ?BK),
    Nb4 = set_field(Nb3, 8, 8, ?OO),
    State#board_state{
      bk_castled = true,
      to_move = ?NEG_COL(ToMove),
      last_move = Op,
      board = Nb4
    }.

-spec set_field(Board, X, Y, Piece) -> Result when
      Board :: board(),
      X :: integer(),
      Y :: integer(),
      Piece :: chess_piece(),
      Result :: board().
set_field(Board, X, Y, Piece) ->
    Pos = ?POS(X, Y),
    setelement(Pos, Board, Piece).

-spec find_first_piece(Board, ChessPiece) -> Result when
      Board :: board(),
      ChessPiece :: chess_piece(),
      Result :: position().
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

