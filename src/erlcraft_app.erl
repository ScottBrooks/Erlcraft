%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the erlcraft application.

-module(erlcraft_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2, stop/1]).


ensure_tables_created() ->
    ok.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erlcraft.
start(_Type, _StartArgs) ->
    erlcraft_deps:ensure(),
    mnesia:start(),
    ensure_tables_created(),
    erlcraft_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erlcraft.
stop(_State) ->
    ok.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
