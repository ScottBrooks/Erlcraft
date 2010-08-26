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
    io:format("PML: [~p,~p,~p] [~p,~p,~p]~n", [X,Y,Z, Stance,Rotation,Pitch]),
    mc_util:write_packet(16#0D, [{double, X}, {double, Y}, {double, Stance}, {double, Z}, {float, Rotation}, {float, Pitch}, {bool, 0}]).

timestamp(Time) ->
    mc_util:write_packet(16#4, [{long, Time}]).

generate_pre_chunk(X,Z, Update) ->
    mc_util:write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
    Data = mc_util:chunk_data(SizeX * SizeY * SizeX),
    Compressed = zlib:compress(mc_util:encode_list(Data)),

    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).

teleport(PlayerID, X, Y, Z, R, P) ->
    mc_util:write_packet(16#22, [{int, PlayerID}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}]).
entity_spawn(EntityID, X, Y, Z, R, P) ->
    mc_util:write_packet(16#15, [{int, EntityID}, {short, 4}, {byte, 1}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}, {byte, 12}]).

entity_add_mob(EntityID, Type, X, Y, Z, R, P) ->
    mc_util:write_packet(16#18, [{int, EntityID}, {byte, Type}, {int, X}, {int, Y}, {int, Z}, {byte, R}, {byte, P}]).

chat(Message) ->
    mc_util:write_packet(16#03, [{string, Message}]).


fake_world(Pid, LocX, LocY, LocZ) ->
    erlcraft_client_fsm:send_packet(Pid, mc_reply:chat("Hello world")),
    lists:map(
        fun(I) ->
            X = trunc(I/8),
            Z = I - X * 8,
            PreChunk = generate_pre_chunk(trunc(LocX + X), trunc(LocZ + Z), 1),
            erlcraft_client_fsm:send_packet(Pid, PreChunk),
            Chunk = generate_chunk(trunc(X),0,trunc(Z), 16,128,16),
            erlcraft_client_fsm:send_packet(Pid, Chunk)
        end,
        lists:seq(0,1)),
    erlcraft_client_fsm:send_packet(Pid, mc_reply:entity_spawn(1, trunc(LocX)*32, trunc(LocY)*32, trunc(LocZ)*32, 0, 0)),
    ok.

