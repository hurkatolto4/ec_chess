#!/usr/bin/env escript -c
%% -*- erlang -*-
%%! -pa apps/ec_chess/ebin -pa apps/ec_prof/ebin

main([]) ->
    io:format("\n\n\nProfiling the code with fprof\n", []),
    io:format("**************************************************\n", []),
    St = ec_board:start_board(),
    [code:load_file(M) || M <- [ec_eval, ec_board]],
    ec_prof:fprof(fun() ->  ec_board:op_cond(St, {{2,2}, {2,4}}, true) end, 
                  "start_board_opcond"),
    ec_prof:fprof(fun() -> ec_eval:eval(St) end,
                  "eval_start_state").

