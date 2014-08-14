-ifndef(__EC_PERF__).
-define(__EC_PERF__, true).

-export([iterate/3]).

-define(LOG_PERF(Str, Count, Time),
    io:format("~40s     - ~p~n", [Str, trunc(Count * (1000000 / Time))])).

-spec iterate(C, Function, Args) -> Result when
    C :: non_neg_integer(),
    Function :: atom(),
    Args :: list(),
    Result :: ok.
iterate(0, _Function, _Args) ->
    ok;
iterate(C, Function, Args) ->
    apply(?MODULE, Function, Args),
    iterate(C - 1, Function, Args).

-endif.
