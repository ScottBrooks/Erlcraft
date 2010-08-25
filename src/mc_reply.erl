-module(mc_reply).

-compile(export_all).

handshake(_RequirePass = true) ->
    mc_util:write_packet(2, [{string, "+"}]);
handshake(_RequirePass = false) ->
    mc_util:write_packet(2, [{string, "-"}]).

login(PlayerID, ServerName, MOTD) ->
    mc_util:write_packet(1, [{int, PlayerID}, {string, ServerName}, {string, MOTD}]).

keepalive() ->
    mc_util:write_packet(0, []).

position_and_look(X, Y, Z, Stance, Rotation, Pitch) ->
    mc_util:write_packet(13, [{double, X}, {double, Y}, {double, Stance}, {double, Z}, {float, Rotation}, {float, Pitch}, {bool, 0}]).

timestamp(Time) ->
    mc_util:write_packet(4, [{long, Time}]).

generate_pre_chunk(X,Z, Update) ->
    mc_util:write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
    Data = mc_util:chunk_data(SizeX * SizeY * SizeX),
    Compressed = zlib:compress(mc_util:encode_list(Data)),

    mc_util:write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).


fake_world(Pid) ->
    lists:map(
        fun(I) ->
            X = trunc(I/8),
            Z = I - X * 8,
            io:format("Chunk for X: ~p Z: ~p~n", [X, Z]),
            PreChunk = generate_pre_chunk(X,Z, 1),
            gen_fsm:send_all_state_event(Pid, {packet, PreChunk}),
            Chunk = generate_chunk(X,0,Z, 16,128,16),
            gen_fsm:send_all_state_event(Pid, {packet, Chunk})
        end,
        lists:seq(-15,15)),
    Pos = mc_reply:position_and_look(0, 100, 0, 0, 0, 0),
    gen_fsm:send_all_state_event(Pid, {packet, Pos}),
    Time = mc_reply:timestamp(1000),
    gen_fsm:send_all_state_event(Pid, {packet, Time}),
    ok.

