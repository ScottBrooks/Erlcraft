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
