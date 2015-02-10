-module(ec_prof_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ec_prof_sup:start_link().

stop(_State) ->
    ok.
