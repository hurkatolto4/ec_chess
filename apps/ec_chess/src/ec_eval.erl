%%------------------------------------------------------------------------------
%% @doc Evaluating functions
%%------------------------------------------------------------------------------
-module(ec_eval).

-ifdef(NATIVE_COMPILE).
-compile([native, inline, {inline_size, 300}]).
-endif.

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
    case ec_board:is_check_mate(State) of
        true ->
            %% one of the players is mated
            case ToMove of
                ?BLACK -> ?MATE_VALUE;
                ?WHITE -> -?MATE_VALUE
            end;
        false ->
            case ec_board:is_stale_mate(State) of
                true -> 0;
                false -> eval_board(1, Board, 0)
            end
    end.

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


