-module(mc_world).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {world_path, world, chunk_store, block_store, clients, world_updates}).
-define(SERVER_TIMEOUT, 30000).

%% External API
-export([start_link/1, get_chunk/3, get_spawn/0, load_chunk/5, dbg_chunk/3, block_dig/6, register_client/2]).

generate_pre_chunk(X,Z, Update) ->
    mc_util:write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

block_from_bin(Binary, Offset) ->
    <<_:Offset/binary, Block:8/integer, _/binary>> = Binary,
    Block.
block_from_bin_packed(Binary, Offset) ->
    AdjOffset = Offset * 4,
    <<_:AdjOffset/bits, Block:8/integer, _/bits>> = Binary,
    Block bsr 4.

%generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
%    Data = mc_util:chunk_data(SizeX * SizeY * SizeX),
%    Compressed = zlib:compress(mc_util:encode_list(Data)),
%    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).
block_dig(State, _X, _Y, _Z, _Direction, _Client, _ChunkStore) when State =:= 0 ->
    ok;
block_dig(State, _X, _Y, _Z, _Direction, _Client, _ChunkStore) when State =:= 1 ->
    ok;
block_dig(State, _X, _Y, _Z, _Direction, _Client, _ChunkStore) when State =:= 2 ->
    ok;
block_dig(State, X, Y, Z, Direction, _Client, ChunkStore) when State =:= 3 ->
    io:format("Dig: S: ~p [~p,~p,~p], D: ~p~n", [State, X, Y, Z, Direction]),
    Key = {X, Z},
    case ets:lookup(ChunkStore, Key) of
        [] ->
            io:format("Key: ~p not found[~p,~p,~p]~n", [Key, X, Y, Z]);
        [{Key, BD, MD, WL}] ->
            Offset = Y,
            HalfOffset = trunc(Y/2),
            Block = block_from_bin(BD, Offset),
            Meta = block_from_bin_packed(MD, HalfOffset),
            io:format("B: ~p M: ~p~n", [Block, Meta]),
            <<Before:Offset/binary, Block:8/integer, After/binary>> = BD,
            NBD = <<Before/binary, 0:8/integer, After/binary>>,
            ets:insert(ChunkStore, {Key, NBD, MD, WL}),
            % Random location inside the block that was broken
            RX = X * 32 + 8 + random:uniform() * 16,
            RZ = Z * 32 + 8 + random:uniform() * 16,
            [{block, X, Y, Z, 0, 8}, {spawn, random:uniform(8192), Block, 1, RX, (Y+1) * 32, RZ, 0, 0, 0}];
        Else ->
            io:format("unknown: ~p key: ~p~n", [Else, Key]),
            ok
    end.

load_chunk(X, Y, Z, Root, ChunkStore) ->
    SizeX = 16, SizeY = 128, SizeZ = 16,
    [BlockData, MetaData, LightData] = try ets:member(ChunkStore, {X*16, Z*16}) of
        true -> load_chunk_ets(X, Y, Z, ChunkStore);
        false-> load_chunk_disk(X, Y, Z, Root, ChunkStore)
        catch _:_ ->
            load_chunk_disk(X, Y, Z, Root, ChunkStore) 
    end,
    Compressed = zlib:compress(<<BlockData/binary, MetaData/binary, MetaData/binary, LightData/binary>>),
    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).


load_chunk_ets(X, _Y, Z, ChunkStore) ->
    lists:foldl(fun(Idx, [Blocks, Meta, Light]) -> 
            OZ = Idx rem 16,
            OX = trunc(Idx/16),
            case ets:lookup(ChunkStore, {X * 16 + OX, Z * 16 + OZ}) of
                [{{_,_}, BD, MD, WL}] ->
                    [<<Blocks/binary, BD/binary>>, <<Meta/binary, MD/binary>>, <<Light/binary, WL/binary>>];
                _ -> io:format("Could not find key: [~p,~p]", [X*16 + OX, Z*16 + OZ])
            end
        end, [<<>>, <<>>, <<>>], lists:seq(0, 255)).

load_chunk_disk(X, _Y, Z, Root, ChunkStore) ->
    F1 = string:to_lower(case X of
        PosX when PosX >= 0 ->
            erlang:integer_to_list(X rem 64, 36);
        _ ->
            erlang:integer_to_list((64+X) rem 64, 36)
    end),
    F2 = string:to_lower(case Z of
        PosZ when PosZ >= 0 ->
            erlang:integer_to_list(Z rem 64, 36);
        _ ->
            erlang:integer_to_list((64+Z) rem 64, 36)
    end),
    FileName = string:to_lower(string:join(["c", erlang:integer_to_list(X, 36), erlang:integer_to_list(Z, 36), "dat"], ".")),
    Path = string:join([Root, F1, F2, FileName], "/"),
    Data = nbt:load_file(Path),
    {tag_compound, <<"Level">>, LevelData} = Data,
    {tag_byte_array, <<"Blocks">>, Blocks} = lists:keyfind(<<"Blocks">>, 2, LevelData),
    {tag_byte_array, <<"BlockLight">>, BlockLight} = lists:keyfind(<<"BlockLight">>, 2, LevelData),
    {tag_byte_array, <<"SkyLight">>, SkyLight} = lists:keyfind(<<"SkyLight">>, 2, LevelData),
    {tag_byte_array, <<"Data">>, MetaData} = lists:keyfind(<<"Data">>, 2, LevelData),
    WorldLight = mc_util:or_binaries(BlockLight, SkyLight),
    lists:foreach(fun(Idx) -> 
            BlockIdx = Idx * 128,
            MetaIdx = Idx * 64,
            OZ = Idx rem 16,
            OX = trunc(Idx/16),
            <<_:BlockIdx/binary, BD:128/binary, _/binary>> = Blocks,
            <<_:MetaIdx/binary, MD:64/binary, _/binary>> = MetaData,
            <<_:MetaIdx/binary, WL:64/binary, _/binary>> = WorldLight,
            true = ets:insert(ChunkStore, {{X * 16 + OX,Z * 16 + OZ}, BD, MD, WL})
        end, lists:seq(0, 255)),
    io:format("Wrote [~p,~p] to [~p, ~p]~n", [X*16, Z*16, X*16+16, Z*16+16]),
    [Blocks, MetaData, WorldLight].

dbg_chunk(<<>>, <<>>, <<>>) ->
    ok;
dbg_chunk(Blocks, MetaData, LightData) ->
    <<Block:8/integer, RestBlocks/binary>> = Blocks,
    %io:format("Block: ~p~n", [Block]),
    <<Meta:8/integer, RestMeta/binary>> = MetaData,
    %io:format("Meta: ~p~n", [Meta]),
    <<Light:4/bits, RestLight/bits>> = LightData,
    %io:format("Light: ~p~n", [Light]),
    case Block of
        71 ->
            io:format("Iron door: ~p M: ~p L: ~p~n", [Block, Meta, Light]);
        65 ->
            io:format("Ladder: ~p M: ~p L: ~p~n", [Block, Meta, Light]);
        50 ->
            io:format("Torch: ~p M: ~p L: ~p~n", [Block, Meta, Light]);
        _ -> ok
    end,
    dbg_chunk(RestBlocks, RestMeta, RestLight).

block_dig(Stage, X, Y, Z, Direction, Client) ->
    gen_server:call(?MODULE, {block_dig, Stage, X, Y, Z, Direction, Client}, ?SERVER_TIMEOUT).

get_spawn() ->
    gen_server:call(?MODULE, {get_spawn}, ?SERVER_TIMEOUT).

get_chunk(X,Y,Z) ->
    gen_server:call(?MODULE, {get_chunk, trunc(X), trunc(Y), trunc(Z)}, ?SERVER_TIMEOUT).

register_client(Client, Details) ->
    gen_server:call(?MODULE, {register_client, Client, Details}, ?SERVER_TIMEOUT).

start_link(World) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [World], []).


%% gen_server events

init([MapName]) ->
    io:format("World Server started~n", []),
    {ok, Cwd} = file:get_cwd(),
    Path = string:join([Cwd, MapName] , "/"),
    LevelPath = string:join([Path, "level.dat"], "/"),
    World = nbt:load_file(LevelPath),
    ChunkStore = ets:new(world, []),
    BlockStore = ets:new(blocks, []),
    timer:send_after(500, flush_world_updates),
    {ok, #state{world_path = Path, world = World, chunk_store = ChunkStore, clients = [], world_updates = [], block_store = BlockStore}}.

handle_call({register_client, Client, Details}, _From, #state{clients = Clients} = State) when is_pid(Client) ->
    lists:foreach(fun(C) ->
            {client, ClientPid} = C,
            {client_details, ID, Name, X, Y, Z, R, P, I} = Details,
            io:format("Sending named spawn[~p]: ~p to ~p~n", [ID, Name, ClientPid]),
            gen_server:cast(ClientPid, {packet, mc_reply:named_spawn(ID, Name, X ,Y, Z, R, P, I)})
        end, Clients),
    NewClients = lists:keystore(Client, 2, Clients, {client, Client}),
    io:format("Registering client: ~p~n", [Client]),
    {reply, ok, State#state{clients = NewClients}};

handle_call({get_chunk, X, Y, Z}, _From, #state{world_path = WorldPath, chunk_store = ChunkStore} = State) when is_integer(X), is_integer(Z) ->
    io:format("Chunk Request: [~p, ~p, ~p]~n", [X, Y, Z]),
    PreChunk = generate_pre_chunk(X, Z, 1),
%    ChunkData = generate_chunk(X, Y, Z, 16, 128, 16),
    ChunkData = load_chunk(X, Y, Z, WorldPath, ChunkStore),
    {reply, {chunk, PreChunk, ChunkData}, State};

handle_call({get_spawn}, _From, #state{world = World} = State) ->
    {tag_compound, <<"Data">>, Data} = World,
    {SX, SY, SZ} = case lists:keyfind(<<"Player">>, 2, Data) of
        {tag_compound, <<"Player">>, PlayerInfo} ->
            {tag_list, <<"Pos">>, _, [SpawnX, SpawnY, SpawnZ]} = lists:keyfind(<<"Pos">>, 2, PlayerInfo),
            {SpawnX, SpawnY, SpawnZ};
        _ ->
            SpawnX = case lists:keyfind(<<"SpawnX">>, 2, Data) of
                {tag_int, <<"SpawnX">>, X} -> X;
                _ -> 0
            end,
            SpawnY = case lists:keyfind(<<"SpawnY">>, 2, Data) of
                {tag_int, <<"SpawnY">>, Y} -> Y;
                _ -> 96
            end,
            SpawnZ = case lists:keyfind(<<"SpawnZ">>, 2, Data) of
                {tag_int, <<"SpawnZ">>, Z} -> Z;
                _ -> 0
            end,
            {SpawnX, SpawnY, SpawnZ}
    end,
    io:format("Spawning player at: [~p, ~p, ~p]~n", [SpawnX, SpawnY, SpawnZ]),
    {reply, {loc, SX, SY, SZ, SY - 1.5, 0.0, 0.0}, State};

handle_call({block_dig, Stage, X, Y, Z, Direction, Client}, _From, #state{chunk_store = ChunkStore, world_updates = WorldUpdates} = State) ->
    NewUpdates = case block_dig(Stage, X, Y, Z, Direction, Client, ChunkStore) of
        ok ->
            WorldUpdates;
        Update -> lists:flatten([Update | WorldUpdates])
    end,
    {reply, none, State#state{world_updates = NewUpdates}};

handle_call(_Request, _From, _State) ->
    io:format("Call: ~p~n", [_Request]),
    {reply, none, _State}.

handle_cast(_Request, _State) ->
    io:format("Cast: ~p~n", [_Request]),
    {noreply, _State}.

handle_info(flush_world_updates, #state{clients = Clients, world_updates = WorldUpdates, block_store = BlockStore} = State) ->
    lists:foreach(fun(Item) ->
            Packet = case Item of 
                {block, X, Y, Z, Type, Meta} ->
                    mc_reply:block_change(trunc(X), trunc(Y), trunc(Z), Type, Meta);
                {spawn, ID, ItemID, U1, X, Y, Z, R, P, U2} ->
                    Key = {trunc(X/32), trunc(Z/32)},
                    io:format("Spawn block at: ~p~n", [Key]),
                    case ets:lookup(BlockStore, Key) of
                        [] ->
                            ets:insert(BlockStore, {Key, [{ID, ItemID}]});
                        {Key, Values} ->
                            ets:insert(BlockStore, {Key, [{ID, ItemID}|Values]})
                    end,
                    mc_reply:item_spawn(ID, ItemID, U1, trunc(X), trunc(Y), trunc(Z), R, P, U2)
            end,
            lists:foreach(fun(Client) ->
                    {client, ClientPid} = Client,
                    gen_server:cast(ClientPid, {packet, Packet})
                end, Clients)
        end, WorldUpdates),
    BlocksSent = lists:foldl(fun(Client, Acc) ->
            {client, ClientPid} = Client,
            {loc, X, _Y, Z} = gen_server:call(ClientPid, {get_location}, infinity),
            Key = {trunc(X), trunc(Z)},
            io:format("Key: ~p~n", [Key]),
            case ets:lookup(BlockStore, Key) of
                [{Key, Values}] ->
                    io:format("Found block for client~n", []),
                    [gen_server:cast(ClientPid, {give_item, ID, ItemID}) || {ID, ItemID} <- Values],
                    [Key | Acc];
                _ -> Acc
            end
        end, [], Clients),
    [ets:delete(BlockStore, Key) || Key <- BlocksSent],
    timer:send_after(500, flush_world_updates),
    {noreply, State#state{world_updates = []}};

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

