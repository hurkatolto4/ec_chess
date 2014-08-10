-module(ec_chess_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([get_conf/2,
         get_conf/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

get_conf(Area, Key) ->
    case application:get_application() of
        undefined -> {error, no_such_application};
        {ok, App} -> get_conf(App, Area, Key)
    end.

get_conf(App, Area, Key) ->
    case application:get_env(App, Area) of
        undefined -> {error, no_such_area};
        {ok, Config} ->
            case lists:keyfind(Key, 1, Config) of
                false -> {error, no_such_key};
                {_, Value} -> {ok, Value}
            end
    end.



start(_StartType, _StartArgs) ->
    ec_chess_sup:start_link().

stop(_State) ->
    ok.
