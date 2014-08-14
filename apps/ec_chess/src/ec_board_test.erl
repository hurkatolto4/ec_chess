%%------------------------------------------------------------------------------
%% eunit tests for ec_board
%%------------------------------------------------------------------------------
-module(ec_board_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(octest(Exp, State, Op),
        ?assertEqual(Exp, ec_board:op_cond(State, Op, true))
    ).

init_test() ->
    St = ec_board:start_board(),

    %% Pawn simple moves
    ?octest(true, St, {{2,2},{2,3}}),
    ?octest(true, St, {{2,2},{2,4}}),
    ?octest(true, St, {{3,2},{3,4}}),
    % Negative
    ?octest(false, St, {{3,2},{3,5}}),  %% can't move 3
    ?octest(false, St, {{3,2},{4,3}}),

    % Knight
    ?octest(true, St, {{2,1},{1,3}}),
    ?octest(true, St, {{2,1},{3,3}}),
    ?octest(true, St, {{7,1},{6,3}}),
    ?octest(true, St, {{7,1},{8,3}}),
    % Negative
    ?octest(false, St, {{2,1},{4,2}}),
    ?octest(false, St, {{2,1},{8,3}}).

-endif.
