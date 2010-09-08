-module(mc_world).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {world_path, world}).

%% External API
-export([start_link/1, get_chunk/3, get_spawn/0, load_chunk/4, dbg_chunk/3]).

generate_pre_chunk(X,Z, Update) ->
    mc_util:write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

%generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
%    Data = mc_util:chunk_data(SizeX * SizeY * SizeX),
%    Compressed = zlib:compress(mc_util:encode_list(Data)),
%    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).

load_chunk(X,Y,Z, Root) ->
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
    SizeX = 16, SizeY = 128, SizeZ = 16,
    %MetaInfo = mc_util:expand_4_to_8(MetaData),
    WorldLight = mc_util:or_binaries(BlockLight, SkyLight),
    Compressed = zlib:compress(<<Blocks/binary, MetaData/binary, MetaData/binary, WorldLight/binary>>),
    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).

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

get_spawn() ->
    gen_server:call(?MODULE, {get_spawn}).

get_chunk(X,Y,Z) ->
    gen_server:call(?MODULE, {get_chunk, trunc(X), trunc(Y), trunc(Z)}).

start_link(World) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [World], []).


%% gen_server events

init([MapName]) ->
    io:format("World Server started~n", []),
    {ok, Cwd} = file:get_cwd(),
    Path = string:join([Cwd, MapName] , "/"),
    LevelPath = string:join([Path, "level.dat"], "/"),
    World = nbt:load_file(LevelPath),
    {ok, #state{world_path = Path, world = World}}.

handle_call({get_chunk, X, Y, Z}, _From, #state{world_path = WorldPath} = State) when is_integer(X), is_integer(Z) ->
    io:format("Chunk Request: [~p, ~p, ~p]~n", [X, Y, Z]),
    PreChunk = generate_pre_chunk(X, Z, 1),
%    ChunkData = generate_chunk(X, Y, Z, 16, 128, 16),
    ChunkData = load_chunk(X, Y, Z, WorldPath),
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

