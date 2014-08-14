%%------------------------------------------------------------------------------
%% @doc Evaluating functions
%%------------------------------------------------------------------------------
-module(ec_eval).

-compile([native, inline, {inline_size, 300}]).

-export([eval/1]).

-include("ec.hrl").
-include("ec_perf.hrl").

%%------------------------------------------------------------------------------
%% API
%%------------------------------------------------------------------------------

-spec eval(State :: #board_state{}) -> Result :: integer().
eval(State) ->
    #board_state{to_move = ToMove,
                 board = Board} = State,
    eval_board(1, Board, 0).

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

eval_board(65, _Board, Res) ->
    Res;
eval_board(P, Board, Res) ->
    eval_board(P + 1, Board, Res + get_piece_value(Board, P)).

get_piece_value(Board, Pos) ->
    get_piece_value(element(Pos, Board)).

get_piece_value(?OO) -> 0;
get_piece_value(?WP) -> 1;
get_piece_value(?WN) -> 3;
get_piece_value(?WB) -> 3;
get_piece_value(?WR) -> 5;
get_piece_value(?WQ) -> 9;
get_piece_value(?WK) -> 0;
get_piece_value(?BP) -> -1;
get_piece_value(?BN) -> -3;
get_piece_value(?BB) -> -3;
get_piece_value(?BR) -> -5;
get_piece_value(?BQ) -> -9;
get_piece_value(?BK) -> 0.


