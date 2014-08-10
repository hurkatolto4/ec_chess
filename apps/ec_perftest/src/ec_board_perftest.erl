-module(ec_board_perftest).

-export([start/1]).

-include_lib("ec_chess/include/ec.hrl").

start(Count) ->
    L = lists:seq(1, Count),
    init_board_test(Count, L).

init_board_test(C, L) ->
    St = ec_board:start_board(),
    ec_board:perftest(St, C, L).

%    {T1, Res} = timer:tc(fun() -> [ec_board:op_cond(St, {{2,2},{2,4}}) || _ <- L] end),
%    io:format("~p ~p Res: '~p' ~n", [?MODULE, ?LINE, Res]),
%    io:format("~p ~p T1: '~p' ~n", [?MODULE, ?LINE, T1]).

