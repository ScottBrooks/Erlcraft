%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for erlcraft.

-module(erlcraft_server).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    Req:dump(),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "announce" ->
                    tracker:announce(Req);
                "scrape" ->
                    tracker:scrape(Req);
%                "purge" ->
%                    io:format("Purging all torrents~n", []),
%                    tracker:purge(),
%                    Req:respond({200, [], ["ok"]});
                _       ->
                    Req:not_found()
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
