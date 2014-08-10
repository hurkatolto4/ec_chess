#!/usr/bin/env escript -c
%% -*- erlang -*-
%%! -pa apps/ec_chess/ebin -pa apps/ec_perftest/ebin

main([]) ->
    ec_board_perftest:start(1000000).
    

