-module(erlcraft_client).

-behaviour(gen_server).


%% Gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {fsm, timer_ref, client_start, loc, chunk_list, player_id}).


update_chunks(ChunkX, ChunkY, ChunkZ, FSMPid, ChunkList) ->
    NewX = trunc(ChunkX/16),
    NewY = trunc(ChunkY/128),
    NewZ = trunc(ChunkZ/16),
    Xs = lists:seq(NewX-3, NewX+3),
    Zs = lists:seq(NewZ-3, NewZ+3),
    Keys = lists:flatten(
        lists:map(
            fun(X) ->
                lists:map(fun(Z) -> {pos, X, NewY, Z} end, Zs)
            end, Xs)),
    ChunksToLoad = lists:filter(fun(Key) -> sets:is_element(Key, ChunkList) =:= false end, Keys),
    lists:foldl(fun(Key, Acc) ->
            {pos, CX, CY, CZ} = Key,
            {chunk, PreChunk, Chunk} = mc_world:get_chunk(CX, CY, CZ),
            erlcraft_client_fsm:send_packet(FSMPid, PreChunk),
            erlcraft_client_fsm:send_packet(FSMPid, Chunk),
            sets:add_element(Key, Acc)
        end, ChunkList, ChunksToLoad).

init([FSMPid]) ->
    io:format("Started!~n", []),
    {ok, TRef} = timer:send_interval(1000, update_time),
    ChunkList = sets:new(),
    {ok, #state{fsm = FSMPid, timer_ref = TRef, client_start = now(), loc = undefined, chunk_list = ChunkList, player_id = undefined}}.

handle_call(_Request, _From, _State) ->
    io:format("Call: ~p~n", [_Request]),
    {reply, none, _State}.

handle_cast({move_look, X,Y,Z, S, R, P}, #state{fsm = FSMPid, loc = Location, chunk_list = ChunkList} = State) ->
    {loc, NewX, NewY, NewZ, NewStance, NewRotation, NewPitch, NewList} = case Location of
        undefined ->
            {loc, SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch} = mc_world:get_spawn(),
            NewChunkList = update_chunks(SpawnX, SpawnY, SpawnZ, FSMPid, ChunkList),
            erlcraft_client_fsm:send_packet(FSMPid, mc_reply:position_and_look(SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch)),
            {loc, SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch, NewChunkList};
        _ ->
            NewChunkList = update_chunks(X, Y, Z, FSMPid, ChunkList),
            {loc, X, Y, Z, S, R, P, NewChunkList}
    end,
    {noreply, State#state{loc = {NewX, NewY, NewZ, NewStance, NewRotation, NewPitch}, chunk_list = NewList}};

handle_cast({position, X, Y, Z, S, _U}, #state{loc = {_, _, _, _, R, P}, fsm = FSMPid, chunk_list = ChunkList} = State) ->
    NewList = update_chunks(X, Y, Z, FSMPid, ChunkList),
    {noreply, State#state{loc = {X, Y, Z, S, R, P}, chunk_list = NewList}};
handle_cast({look, R, P, _U}, #state{loc = {X, Y, Z, S, _, _}} = State) ->
    {noreply, State#state{loc = {X, Y, Z, S, R, P}}};

handle_cast({player_id, PlayerID}, State) ->
    {noreply, State#state{player_id = PlayerID}};

handle_cast({flying, _Flying}, State) ->
    {noreply, State};

handle_cast({kick, Message}, State) ->
    io:format("Kick: ~p~n", [Message]),
    {stop, normal, State};
handle_cast(_Request, _State) ->
    io:format("Cast: ~p~n", [_Request]),
    {noreply, _State}.


handle_info(update_time, #state{fsm = FSMPid, client_start = ClientStart} = State) ->
    Timestamp = trunc(timer:now_diff(now(), ClientStart)/1000000)*20,

    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:timestamp(Timestamp)),
    
    {noreply, State};

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
