%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(erlcraft).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the erlcraft server.
start() ->
    erlcraft_deps:ensure(),
    ensure_started(crypto),
    application:start(erlcraft).

%% @spec stop() -> ok
%% @doc Stop the erlcraft server.
stop() ->
    Res = application:stop(erlcraft),
    application:stop(crypto),
    Res.
