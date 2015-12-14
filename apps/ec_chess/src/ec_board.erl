-module(ec_board).

-ifdef(NATIVE_COMPILE).
-compile([native, inline, {inline_size, 300}]).
-endif.

-export([
    op_cond/3,
    init_board/0,
    start_board/0,
    is_check_mate/1,
    is_stale_mate/1
]).

-include("ec.hrl").
-include("ec_perf.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec op_cond(State, Op, CheckAfter) -> Result when
    State :: #board_state{},
    Op :: operator(),
    CheckAfter :: boolean(),
    Result :: op_cond_res().
op_cond(#board_state{} = State, Op, CheckAfter) ->
    case check_not_equal(Op) of
        ok ->
            case check_field_limits(Op) of
                ok ->
                    op_cond2(State, Op, CheckAfter);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec start_board() ->
          #board_state{}.
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

-spec is_check_mate(State) -> Result when
    State :: #board_state{},
    Result :: boolean().
is_check_mate(#board_state{to_move = ToMove} = State) ->
    %% check mate: if actually in check and there is no operator which
    %% takes us out from check
    King = case ToMove of
               ?WHITE -> ?WK;
               ?BLACK -> ?BK
           end,
    KingPos = find_first_piece(State#board_state.board, King),
    IsInCheck = is_in_check(1, State#board_state{to_move = ?NEG_COL(ToMove)},
                            KingPos, ?NEG_COL(ToMove)),
    IsInCheck andalso cant_move_out_from_check(1, State).

-spec is_stale_mate(State) -> Result when
    State :: #board_state{},
    Result :: boolean().
is_stale_mate(#board_state{to_move = ToMove} = State) ->
    King = case ToMove of
               ?WHITE -> ?WK;
               ?BLACK -> ?BK
           end,
    KingPos = find_first_piece(State#board_state.board, King),
    IsInCheck = is_in_check(1, State#board_state{to_move = ?NEG_COL(ToMove)},
                            KingPos, ?NEG_COL(ToMove)),
    (not IsInCheck) andalso cant_move_out_from_check(1, State).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

-spec op_cond2(State, Op, CheckAfter) -> op_cond_res() when
    State :: #board_state{},
    Op :: operator(),
    CheckAfter :: boolean().
op_cond2(#board_state{board = Board, to_move = ToMove} = State,
         {{Fx, Fy}, {Tx, Ty}} = Op, CheckAfter) ->
    case {?GET_PIECE(Board, Fx, Fy), ?GET_PIECE(Board, Tx, Ty)} of
        {?OO, _} ->
            %% can not move empty field
            {error, {?ECE_EMPTY_FIELD, ?LINE}};
        {FromPiece, ToPiece} ->
            %% check whether start and end pos have pieces with same colours
            case {color(FromPiece), color(ToPiece)} of
                {?WHITE, ?WHITE} ->
                    {error, {?ECE_SAME_COLOR, ?LINE}};
                {?BLACK, ?BLACK} ->
                    {error, {?ECE_SAME_COLOR, ?LINE}};
                {Col, _} when Col =/= ToMove ->
                    %% not your turn bastard
                    {error, {?ECE_NOT_YOUR_TURN, ?LINE}};
                {Col, _} ->
                    case CheckAfter andalso
                         (ToPiece =:= ?BK orelse ToPiece =:= ?WK) of
                        true ->
                            {error, {?ECE_CANT_TAKE_KING, ?LINE}};
                        false ->
                            is_in_check(State, Op, CheckAfter, FromPiece, Col)
                    end
            end
    end.

-spec op_cond3(FromPiece, State, Op) -> Result when
      State :: #board_state{},
      Op :: operator(),
      FromPiece :: integer(),
      Result :: op_cond_res().
%% White pawn
op_cond3(?WP, State, Op) ->
    Col = color(?WP),
    case is_pawn_simple_move(State, Op, Col) orelse
         is_pawn_take(State, Op, Col) orelse
         is_en_passant(State, Op, Col) of
        true ->
            ok;
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% White rook
op_cond3(?WR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% White knight
op_cond3(?WN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
         (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1) of
        true ->
            ok;
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% White bishop
op_cond3(?WB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% White queen
op_cond3(?WQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% White king
op_cond3(?WK, State, {{Fx, Fy}, {Tx, Ty}} = Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    case (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0) of
        true ->
            ok;
        false ->
            case is_castle(State, Op) of
                true -> ok;
                false -> {error, {?ECE_NOT_VALID, ?LINE}}
            end
    end;
%% Black pawn
op_cond3(?BP, State, Op) ->
    Col = color(?BP),
    case is_pawn_simple_move(State, Op, Col) orelse
         is_pawn_take(State, Op, Col) orelse
         is_en_passant(State, Op, Col) of
        true ->
            ok;
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% Black rook
op_cond3(?BR, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% Black knight
op_cond3(?BN, _State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case (diff(Tx, Fx) =:= 1 andalso diff(Ty, Fy) =:= 2) orelse
         (diff(Tx, Fx) =:= 2 andalso diff(Ty, Fy) =:= 1) of
        true ->
            ok;
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% Black bishop
op_cond3(?BB, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% Black queen
op_cond3(?BQ, State, {{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case diff(Fx, Tx) =:= diff(Fy, Ty) orelse Fx =:= Tx orelse Fy =:= Ty of
        true ->
            check_empty_fields_between(State#board_state.board, Fx, Fy, Tx, Ty);
        false ->
            {error, {?ECE_NOT_VALID, ?LINE}}
    end;
%% Black king
op_cond3(?BK, State, {{Fx, Fy}, {Tx, Ty}} = Op) ->
    Dx = diff(Fx, Tx),
    Dy = diff(Fy, Ty),
    case (Dx =:= 1 orelse Dx =:= 0) andalso (Dy =:= 1 orelse Dy =:= 0) of
        true ->
            ok;
        false ->
            case is_castle(State, Op) of
                true -> ok;
                false -> {error, {?ECE_NOT_VALID, ?LINE}}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Returns true only if all the fields not counting the start and stop
%%      positions are empty between the two coordinates
%%------------------------------------------------------------------------------
-spec check_empty_fields_between(Board, Fx, Fy, Tx, Ty) -> Result when
      Board :: board(),
      Fx :: integer(),
      Fy :: integer(),
      Tx :: integer(),
      Ty :: integer(),
      Result :: op_cond_res().
check_empty_fields_between(Board, Fx, Fy, Tx, Ty) ->
    Dx = get_direction(Fx, Tx),
    Dy = get_direction(Fy, Ty),
    case check_empty_fields(Board, Fx + Dx, Fy + Dy, Dx, Dy, Tx, Ty) of
        true ->
            ok;
        false ->
            {error, {?ECE_PIECE_ON_PATH, ?LINE}}
    end.

check_empty_fields(_Board, CurX, CurY, _DX, _DY, TX, TY) when
      CurX =:= TX, CurY =:= TY ->
    true;
check_empty_fields(Board, CurX, CurY, DX, DY, TX, TY) ->
    case ?GET_PIECE(Board, CurX, CurY) of
        ?OO ->
            check_empty_fields(Board, CurX + DX, CurY + DY, DX, DY, TX, TY);
        _Other ->
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

-spec check_not_equal(Op :: operator()) -> op_cond_res().
check_not_equal({{Fx, Fy}, {Fx, Fy}} = _Op) ->
    {error, {?ECE_SAME_FIELDS, ?LINE}};
check_not_equal(_Op) ->
    ok.

-spec check_field_limits(Op :: operator()) -> op_cond_res().
check_field_limits({{Fx, Fy}, {Tx, Ty}} = _Op) ->
    case between(Fx, 1, 8) andalso between(Fy, 1, 8) andalso
         between(Tx, 1, 8) andalso between(Ty, 1, 8) of
        true -> ok;
        false -> {error, {?ECE_OUT_OF_RANGE, ?LINE}}
    end.

between(V, Min, Max) when V >= Min, V =< Max -> true;
between(_V, _Min, _Max) -> false.

-spec is_in_check(State, Op, CheckAfter, FromPiece, Col) -> Result when
    State :: #board_state{},
    Op :: operator(),
    CheckAfter :: boolean(),
    FromPiece :: chess_piece(),
    Col :: color(),
    Result :: op_cond_res().
is_in_check(State, Op, CheckAfter, FromPiece, Col) ->
    case CheckAfter of
        false ->
            %% don't check whether after operator we moved into
            %% check
            op_cond3(FromPiece, State, Op);
        true ->
            case op_cond3(FromPiece, State, Op) of
                ok ->
                    case check_chess_after_op(State, Op, Col) of
                        true ->
                            {error, {?ECE_CHECK_AFTER_MOVE, ?LINE}};
                        false ->
                            ok
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

%% Returns true if after applying the operator the player who had to move is in
%% check
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

-spec is_in_check(Pos, State, KingPos, OppCol) -> Result when
    Pos :: pos_integer(),
    State :: #board_state{},
    KingPos :: position(),
    OppCol :: color(),
    Result :: boolean().
is_in_check(65, _State, _KingPos, _OppCol) ->
    false;
is_in_check(C, State, KingPos, OppCol) ->
    {Fx, Fy} = FromPos = ?UN_POS(C),
    Col = color(?GET_PIECE(State#board_state.board, Fx, Fy)),
    case Col =:= OppCol of
        true ->
            Op = {FromPos, KingPos},
            case op_cond(State, Op, _CheckAfter = false) of
                ok -> true;
                {error, _} -> is_in_check(C + 1, State, KingPos, OppCol)
            end;
        false ->
            is_in_check(C + 1, State, KingPos, OppCol)
    end.

-spec cant_move_out_from_check(Pos, State) -> Result when
    Pos :: pos_integer(),
    State :: #board_state{},
    Result :: boolean().
cant_move_out_from_check(65, _State) ->
    true;
cant_move_out_from_check(FromPos, State) ->
    case cant_move_out_from_check(FromPos, 1, State) of
        false -> false;
        true -> cant_move_out_from_check(FromPos + 1, State)
    end.

-spec cant_move_out_from_check(FromPos, ToPos, State) -> Result when
    FromPos :: pos_integer(),
    ToPos :: pos_integer(),
    State :: #board_state{},
    Result :: boolean().
cant_move_out_from_check(_FromPos, 65, _State) ->
    true;
cant_move_out_from_check(FromPos, ToPos, State) ->
    From = ?UN_POS(FromPos),
    To = ?UN_POS(ToPos),
    Op = {From, To},
    case op_cond(State, Op, true) of
        ok ->
            %% found operator which brings us out from check
            false;
        {error, _Reason} ->
            cant_move_out_from_check(FromPos, ToPos + 1, State)
    end.

-spec apply_op(State, Op) -> NewState when
      State :: #board_state{},
      Op :: operator(),
      NewState :: #board_state{}.
apply_op(State, {{Fx, Fy}, {_Tx, _Ty}} = Op) ->
    Piece = ?GET_PIECE(State#board_state.board, Fx, Fy),
    case get_castle_type(Piece, Op) of
        ?CASTLE_WHITE_SHORT -> apply_castle_white_short(State, Op);
        ?CASTLE_WHITE_LONG  -> apply_castle_white_long(State, Op);
        ?CASTLE_BLACK_SHORT -> apply_castle_black_short(State, Op);
        ?CASTLE_BLACK_LONG  -> apply_castle_black_long(State, Op);
        ?CASTLE_NOT         ->
            Col =  State#board_state.to_move,
            case is_en_passant(State, Op, Col) of
                true -> apply_en_passant(State, Op);
                false -> apply_simple(State, Op)
            end
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

-spec apply_en_passant(State, Op) -> NewState when
    State :: #board_state{},
    Op :: operator(),
    NewState :: #board_state{}.
apply_en_passant(State, {{Fx, Fy}, {Tx, Ty}} = Op) ->
    Board = State#board_state.board,
    Piece = ?GET_PIECE(Board, Fx, Fy),
    Nb1 = set_field(Board, Fx, Fy, ?OO),
    Nb2 = set_field(Nb1, Tx, Ty, Piece),
    Nb3 = set_field(Nb2, Tx, Fy, ?OO),
    State#board_state{
        to_move = ?NEG_COL(State#board_state.to_move),
        last_move = Op,
        board = Nb3
    }.

apply_simple(State, {{Fx, Fy}, {Tx, Ty}} = Op) ->
    #board_state{to_move = ToMove, board = Board} = State,
    Piece = ?GET_PIECE(State#board_state.board, Fx, Fy),
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
    case element(Pos, Board) of
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
    Result = (Ty - Fy =:= 1 andalso color(?GET_PIECE(B, Tx, Ty)) =:= ?EMPTY) orelse
        (Ty - Fy =:= 2 andalso Fy =:= 2 andalso
            color(?GET_PIECE(B, Tx, Ty)) =:= ?EMPTY andalso
            color(?GET_PIECE(B, Tx, Ty - 1)) =:= ?EMPTY),
    Result;
is_pawn_simple_move(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?BLACK) when Fx =:= Tx ->
    B = State#board_state.board,
    (Ty - Fy =:= -1 andalso color(?GET_PIECE(B, Tx, Ty)) =:= ?EMPTY) orelse
        (Ty - Fy =:= -2 andalso Fy =:= 7 andalso
            color(?GET_PIECE(B, Tx, Ty)) =:= ?EMPTY andalso
            color(?GET_PIECE(B, Tx, Ty + 1)) =:= ?EMPTY);
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
    color(?GET_PIECE(State#board_state.board, Tx, Ty)) =:= ?BLACK;
is_pawn_take(State, {{Fx, Fy}, {Tx, Ty}} = _Op, ?BLACK)
  when abs(Fx - Tx) =:= 1, Ty - Fy =:= -1 ->
    color(?GET_PIECE(State#board_state.board, Tx, Ty)) =:= ?WHITE;
is_pawn_take(_State, _Op, _Col) ->
    false.

%% TODO
%% True when it is an en passant move
-spec is_en_passant(State, Op, Col) -> Result when
      State :: #board_state{},
      Op :: operator(),
      Col :: color(),
      Result :: boolean().
is_en_passant(State, {{Fx, Fy}, {Tx, Ty}} = _Op, _Col = ?WHITE) ->
    case Ty - Fy =:= 1 andalso abs(Fx - Tx) =:= 1 andalso Ty =:= 6 of
        true ->
            case State#board_state.last_move of
                undefined ->
                    false;
                {{_LFx, _LFy}, {LTx, LTy}} ->
                    case Tx =:= LTx andalso Ty =:= LTy + 1 of
                        true ->
                            Board = State#board_state.board,
                            ?GET_PIECE(Board, LTx, LTy) =:= ?BP;
                        false ->
                            false
                    end
            end;
        false ->
            false
    end;
is_en_passant(State, {{Fx, Fy}, {Tx, Ty}} = _Op, _Col = ?BLACK) ->
    case Ty - Fy =:= -1 andalso abs(Fx - Tx) =:= 1 andalso Ty =:= 3 of
        true ->
            case State#board_state.last_move of
                {{_LFx, _LFy}, {LTx, LTy}} ->
                    case Tx =:= LTx andalso Ty =:= LTy - 1 of
                        true ->
                            Board = State#board_state.board,
                            ?GET_PIECE(Board, LTx, LTy) =:= ?BP;
                        false ->
                            false
                    end
            end;
        false ->
            false
    end.

-spec is_castle(State, Op) -> Result when
    State :: #board_state{},
    Op :: operator(),
    Result :: boolean().
%% White short castle
is_castle(#board_state{to_move = ?WHITE, wk_castled = false, board = B} = State,
          {{5, 1}, {7, 1}} = Op) when ?GET_PIECE(B, 8, 1) =:= ?WR ->
    Nb1 = set_field(B, 5, 1, ?OO),
    Nb2 = set_field(Nb1, 6, 1, ?WK),
    Ns = State#board_state{
            wk_castled = true,
            to_move = ?BLACK,
            last_move = Op,
            board = Nb2
         },
    Ns2 = State#board_state{
            wk_castled = true,
            to_move = ?BLACK,
            last_move = Op
          },
    (not is_in_check(1, Ns, {6,1}, ?BLACK)) andalso
        (not is_in_check(1, Ns2, {5,1}, ?BLACK));
%% White long castle
is_castle(#board_state{to_move = ?WHITE, wk_castled = false, board = B} = State,
          {{5, 1}, {3, 1}} = Op) when ?GET_PIECE(B, 1, 1) =:= ?WR ->
    Nb1 = set_field(B, 5, 1, ?OO),
    Nb2 = set_field(Nb1, 4, 1, ?WK),
    Ns = State#board_state{
            wk_castled = true,
            to_move = ?BLACK,
            last_move = Op,
            board = Nb2
         },
    Ns2 = State#board_state{
            wk_castled = true,
            to_move = ?BLACK,
            last_move = Op
          },
    (not is_in_check(1, Ns, {4,1}, ?BLACK)) andalso
        (not is_in_check(1, Ns2, {5,1}, ?BLACK));
%% Black short castle
is_castle(#board_state{to_move = ?BLACK, bk_castled = false, board = B} = State,
          {{5, 8}, {7, 8}} = Op) when ?GET_PIECE(B, 8, 8) =:= ?BR ->
    Nb1 = set_field(B, 5, 8, ?OO),
    Nb2 = set_field(Nb1, 6, 8, ?BK),
    Ns = State#board_state{
            bk_castled = true,
            to_move = ?WHITE,
            last_move = Op,
            board = Nb2
         },
    Ns2 = State#board_state{
            wk_castled = true,
            to_move = ?WHITE,
            last_move = Op
          },
    (not is_in_check(1, Ns, {6,8}, ?WHITE)) andalso
        (not is_in_check(1, Ns2, {5,8}, ?WHITE));
%% Black long castle
is_castle(#board_state{to_move = ?BLACK, bk_castled = false, board = B} = State,
          {{5, 8}, {3, 8}} = Op) when ?GET_PIECE(B, 1, 8) =:= ?BR ->
    Nb1 = set_field(B, 5, 8, ?OO),
    Nb2 = set_field(Nb1, 4, 8, ?BK),
    Ns = State#board_state{
            bk_castled = true,
            to_move = ?WHITE,
            last_move = Op,
            board = Nb2
         },
    Ns2 = State#board_state{
            wk_castled = true,
            to_move = ?WHITE,
            last_move = Op
          },
    (not is_in_check(1, Ns, {4,8}, ?WHITE)) andalso
        (not is_in_check(1, Ns2, {5,8}, ?WHITE));
is_castle(#board_state{} = _St, _Op) ->
    false.

