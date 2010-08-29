-module(mc_world).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% External API
-export([start_link/1, get_chunk/3, get_spawn/0]).

generate_pre_chunk(X,Z, Update) ->
    mc_util:write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
    Data = mc_util:chunk_data(SizeX * SizeY * SizeX),
    Compressed = zlib:compress(mc_util:encode_list(Data)),
    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).



get_spawn() ->
    gen_server:call(?MODULE, {get_spawn}).

get_chunk(X,Y,Z) ->
    gen_server:call(?MODULE, {get_chunk, trunc(X), trunc(Y), trunc(Z)}).

start_link(World) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [World], []).


%% gen_server events

init([_MapName]) ->
    io:format("World Server started~n", []),
    {ok, #state{}}.

handle_call({get_chunk, X, Y, Z}, _From, _State) when is_integer(X), is_integer(Z) ->
    %io:format("Chunk Request: [~p, ~p, ~p]~n", [X, Y, Z]),
    PreChunk = generate_pre_chunk(X, Z, 1),
    ChunkData = generate_chunk(X, Y, Z, 16, 128, 16),
    {reply, {chunk, PreChunk, ChunkData}, _State};

handle_call({get_spawn}, _From, _State) ->
    {reply, {loc, 0, 96, 0, 94.5, 0.0, 0.0}, _State};
 
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

