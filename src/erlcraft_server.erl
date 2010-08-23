%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for erlcraft.

-module(erlcraft_server).
-author('author <author@example.com>').
-behaviour(gen_server).

%% Gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
-export([start/1]).

-record(state, {socket, acceptor}).

start(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_Args) ->
    {ok, LSock} = gen_tcp:listen(1337, [binary, {packet, 0}, {active, once}]),

    State = #state{socket=LSock},
    io:format("Started! ~p~n", [State]),
    {ok, State}.

handle_call(_Request, _From, _State) ->
    io:format("Call: ~p~n", [_Request]),
    {reply, none, _State}.

handle_cast(_Request, _State) ->
    io:format("Cast: ~p~n", [_Request]),
    {noreply, _State}.

handle_info(_Info, _State) ->
    io:format("Info: ~p~n", [_Info]),
    {noreply, _State}.

terminate(_Reason, _State) ->
    io:format("Term: ~p~n", [_Reason]),
    normal.

code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.


%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
-endif.
