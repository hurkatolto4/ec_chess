%% Tuple containing 64 numbers.

-ifndef(__EC_HRL__).
-define(__EC_HRL__, true).

%%
%% Defines
%%

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
-define(EMPTY, 0).
-define(WHITE, 1).
-define(BLACK, 2).

-define(C(X), (8-(X-1))).
-define(POS(X, Y),  ((?C(Y) - 1) * 8 + ?C(X))).
-define(UN_POS(P),  {?C((P-1) rem 8 + 1), ?C((P-1) div 8 + 1)}).

%% Errors occuring during calculations
-define(ERR_NO_SUCH_PIECE, 1).

%%
%% Types
%%
-type board_pos() :: 1..8.
-type chess_piece() :: ?OO..?BK.
-type board() :: tuple().
-type operator() :: {From :: position(), To :: position()}.
-type position() :: {X :: board_pos(), Y :: board_pos()}.  %% must be between 1..8

-type color() :: ?EMPTY | ?WHITE | ?BLACK.

%%
%% Record definitions
%%
-record(board_state, {
    wk_castled = false          :: boolean(),
    bk_castled = true           :: boolean(),
    to_move    = ?WHITE         :: ?WHITE | ?BLACK,
    last_move  = undefined      :: undefined | position(),
    board      = undefined      :: undefined | board()
}).

-endif.
