%% Tuple containing 64 numbers.

-ifndef(__EC_HRL__).
-define(__EC_HRL__, true).

%%
%% Defines
%%

-define(MATE_VALUE, 1000000).

%% Field values
-define(OO, 0).
-define(WP, 1).
-define(WB, 2).
-define(WN, 3).
-define(WR, 4).
-define(WQ, 5).
-define(WK, 6).
-define(BP, 7).
-define(BB, 8).
-define(BN, 9).
-define(BR, 10).
-define(BQ, 11).
-define(BK, 12).

%% Colour codes
-define(WHITE, 0).
-define(BLACK, 1).
-define(EMPTY, 2).

-define(POS(X, Y),  ((8 - Y) * 8 + X)).
-define(UN_POS(P),  {(P - 1) rem 8 + 1  , 8 - (P - 1) div 8}).
-define(NEG_COL(C), 1 - C).
-define(GET_PIECE(Board, X, Y),
    element(?POS(X, Y), Board)
).

%% Errors occuring during calculations
-define(ERR_NO_SUCH_PIECE, 1).
-define(ERR_NOT_EQUAL,     2).
-define(ERR_FIELD_LIMITS,  3).


%%
%% Defines for castle move types
%%
-define(CASTLE_NOT, 0).
-define(CASTLE_WHITE_SHORT, 1).
-define(CASTLE_WHITE_LONG, 2).
-define(CASTLE_BLACK_SHORT, 3).
-define(CASTLE_BLACK_LONG, 4).

%%
%% Types
%%
-type board_pos() :: 1..8.
-type chess_piece() :: ?OO..?BK.
-type board() :: tuple().
-type operator() :: {From :: position(), To :: position()}.
-type position() :: {X :: board_pos(), Y :: board_pos()}.  %% must be between 1..8

-type color() :: ?EMPTY | ?WHITE | ?BLACK.
-type castle_t() :: ?CASTLE_NOT..?CASTLE_BLACK_LONG.

%%
%% Record definitions
%%
-record(board_state, {
    wk_castled = false          :: boolean(),
    bk_castled = false          :: boolean(),
    to_move    = ?WHITE         :: ?WHITE | ?BLACK,
    last_move  = undefined      :: undefined | operator(),
    board      = undefined      :: undefined | board()
}).

%%
%% Error values given back by ec_board
%%
-define(ECE_SAME_FIELDS, same_fields).
-define(ECE_OUT_OF_RANGE, out_of_range).
-define(ECE_EMPTY_FIELD, empty_field).
-define(ECE_SAME_COLOR, same_color).
-define(ECE_NOT_YOUR_TURN, not_your_turn).
-define(ECE_NOT_VALID, not_valid).
-define(ECE_CHECK_AFTER_MOVE, check_after_move).
-define(ECE_PIECE_ON_PATH, piece_on_path).
-define(ECE_CANT_TAKE_KING, cant_take_king).

-type op_cond_res() :: ok | {error, {Reason :: atom(), Line :: integer()}}.

-endif.
