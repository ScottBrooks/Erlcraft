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
            io:format("Asking for Chunk at: ~p~n", [Key]),
            {chunk, PreChunk, Chunk} = mc_world:get_chunk(CX, CY, CZ),
            erlcraft_client_fsm:send_packet(FSMPid, PreChunk),
            erlcraft_client_fsm:send_packet(FSMPid, Chunk),
            io:format("Chunks Sent~n", []),
            sets:add_element(Key, Acc)
        end, ChunkList, ChunksToLoad).

send_world(FSMPid, ChunkList) ->
    io:format("Sending world~n", []),
    {loc, SpawnX, SpawnY, SpawnZ, _SpawnStance, _SpawnRotation, _SpawnPitch} = mc_world:get_spawn(),
    update_chunks(SpawnX, SpawnY, SpawnZ, FSMPid, ChunkList).

init([FSMPid]) ->
    io:format("Started!~n", []),
    {ok, TRef} = timer:send_interval(1000, update_time),
    {ok, #state{fsm = FSMPid, timer_ref = TRef, client_start = now(), loc = undefined, chunk_list = sets:new(), player_id = undefined}}.

handle_call({get_location}, _From, #state{loc = Location} = State) when Location =/= undefined ->
    {X, Y, Z, _S, _R, _P} = Location,
    {reply, {loc, X, Y, Z}, State};

handle_call(_Request, _From, _State) ->
    io:format("Call: ~p~n", [_Request]),
    {reply, none, _State}.


handle_cast({move_look, X,Y,Z, S, R, P}, #state{fsm = FSMPid, chunk_list = ChunkList} = State) ->
    NewChunkList = update_chunks(X, Y, Z, FSMPid, ChunkList),
    {noreply, State#state{loc = {X, Y, Z, S, R, P}, chunk_list = NewChunkList}};

handle_cast({position, X, Y, Z, S, _U}, #state{loc = {_, _, _, _, R, P}, fsm = FSMPid, chunk_list = ChunkList} = State) ->
    NewList = update_chunks(X, Y, Z, FSMPid, ChunkList),
    {noreply, State#state{loc = {X, Y, Z, S, R, P}, chunk_list = NewList}};
handle_cast({look, R, P, _U}, #state{loc = {X, Y, Z, S, _, _}} = State) ->
    {noreply, State#state{loc = {X, Y, Z, S, R, P}}};

handle_cast({client_begin, PlayerID, Username}, #state{fsm = FSMPid, chunk_list = ChunkList} = State) ->
    NewChunkList = send_world(FSMPid, ChunkList),
    {loc, SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch} = mc_world:get_spawn(),
    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:position_and_look(SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch)),
    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:compass(SpawnX, SpawnY, SpawnZ)),
    Details = {client_details, random:uniform(1024), binary_to_list(Username), SpawnX, SpawnY, SpawnZ, SpawnRotation, SpawnPitch, 0},
    mc_world:register_client(self(), Details),
    {noreply, State#state{player_id = PlayerID, chunk_list = NewChunkList, loc = {SpawnX, SpawnY, SpawnZ, SpawnStance, SpawnRotation, SpawnPitch}}};

handle_cast({flying, _Flying}, State) ->
    {noreply, State};

handle_cast({packet, Data}, #state{fsm = FSMPid} = State) ->
    erlcraft_client_fsm:send_packet(FSMPid, Data),
    {noreply, State};

handle_cast({give_item, ID, ItemID}, #state{fsm = FSMPid, player_id = PlayerID} = State) ->
    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:collect_item(ID, PlayerID)),
    erlcraft_client_fsm:send_packet(FSMPid, mc_reply:add_to_inventory(ItemID, 1, 0)),
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
