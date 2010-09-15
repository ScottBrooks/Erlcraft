-module(mc_reply).

-compile(export_all).

handshake(_RequirePass = true) ->
    mc_util:write_packet(16#2, [{string, "+"}]);
handshake(_RequirePass = false) ->
    mc_util:write_packet(16#2, [{string, "-"}]).

login(PlayerID, ServerName, MOTD) ->
    mc_util:write_packet(16#1, [{int, PlayerID}, {string, ServerName}, {string, MOTD}]).

keepalive() ->
    mc_util:write_packet(16#0, []).

position_and_look(X, Y, Z, Stance, Rotation, Pitch) ->
%    io:format("PML: [~p,~p,~p] [~p,~p,~p]~n", [X,Y,Z, Stance,Rotation,Pitch]),
    mc_util:write_packet(16#0D, [{double, X}, {double, Y}, {double, Stance}, {double, Z}, {float, Rotation}, {float, Pitch}, {bool, 0}]).

timestamp(Time) ->
    mc_util:write_packet(16#4, [{long, Time}]).

teleport(PlayerID, X, Y, Z, R, P) ->
    mc_util:write_packet(16#22, [{int, PlayerID}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}]).
entity_spawn(EntityID, X, Y, Z, R, P) ->
    mc_util:write_packet(16#15, [{int, EntityID}, {short, 4}, {byte, 1}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}, {byte, 12}]).

entity_add_mob(EntityID, Type, X, Y, Z, R, P) ->
    mc_util:write_packet(16#18, [{int, EntityID}, {byte, Type}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}]).

chat(Message) ->
    mc_util:write_packet(16#03, [{string, Message}]).

block_change(X, Y, Z, Type, Meta) ->
    mc_util:write_packet(16#35, [{int, X}, {byte,Y}, {int, Z}, {byte, Type}, {byte, Meta}]).

compass(X, Y, Z) ->
    mc_util:write_packet(16#06, [{int, trunc(X)}, {int, trunc(Y)}, {int, trunc(Z)}]).


fake_world(Pid, BlockCount, LocX, _LocY, LocZ) ->
    Width = math:sqrt(BlockCount),
    erlcraft_client_fsm:send_packet(Pid, mc_reply:chat("Hello world")),
    lists:map(
        fun(I) ->
            X = trunc(I/Width),
            Z = I - X * Width,
            ChunkX = trunc(LocX + X - (Width/2)),
            ChunkZ = trunc(LocZ + Z - (Width/2)),
            io:format("Chunk at: [~p, ~p]~n", [ChunkX, ChunkZ]),
            %PreChunk = generate_pre_chunk(ChunkX, ChunkZ, 1),
            %Chunk = generate_chunk(ChunkX,0,ChunkZ, 16,128,16),
            {chunk, PreChunk, Chunk} = mc_world:get_chunk(ChunkX, 0, ChunkZ),
            erlcraft_client_fsm:send_packet(Pid, PreChunk),
            erlcraft_client_fsm:send_packet(Pid, Chunk)
        end,
        lists:seq(0,BlockCount)),
    %erlcraft_client_fsm:send_packet(Pid, mc_reply:entity_spawn(1, trunc(LocX)*32, trunc(LocY)*32, trunc(LocZ)*32, 0, 0)),

    {spawn_location, 0, 66, 0}.
