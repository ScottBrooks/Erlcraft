-module(mc).

-export([handle_packet/2, handle_data/1]).

% Packet macros
% If we have multiple strings in one packet, we need to use the other PKT_STRING2,N macros
-define(PKT_STRING(StrName), _Len:16/integer-signed-big, StrName:_Len/binary).
-define(PKT_STRING2(StrName2), _Len2:16/integer-signed-big, StrName2:_Len2/binary).
-define(PKT_STRING3(StrName3), _Len3:16/integer-signed-big, StrName3:_Len3/binary).
-define(PKT_INTEGER(IntValue), IntValue:32/integer-signed-big).
-define(PKT_FLOAT(FloatValue), FloatValue:32/float).
-define(PKT_FLOAT2(FloatValue2), FloatValue2:32/float).
-define(PKT_DOUBLE(DoubleValue), DoubleValue:64/float).
-define(PKT_BOOL(BoolValue), BoolValue:8).

% Client -> Server

-define(PKT_KEEPALIVE,                       <<0, Rest/binary>>).
-define(PKT_LOGIN(PlayerID, N, P),           <<1, ?PKT_INTEGER(PlayerID), ?PKT_STRING(N), ?PKT_STRING2(P),Rest/binary>>).
-define(PKT_HANDSHAKE(PlayerName),           <<2, ?PKT_STRING(PlayerName), Rest/binary>>).
-define(PKT_LOADED(Loaded),                  <<10, ?PKT_BOOL(Loaded), Rest/binary>>).
-define(PKT_PLAYER_MOVE_LOOK(X,Y,S,Z,R,P,U), <<13, ?PKT_DOUBLE(X), ?PKT_DOUBLE(Y), ?PKT_DOUBLE(S), ?PKT_DOUBLE(Z), ?PKT_FLOAT(R), ?PKT_FLOAT(P), ?PKT_BOOL(U), Rest/binary>>).

handle_data(Data) ->
    case Data of
        % 0 - Keep Alive
        ?PKT_KEEPALIVE -> {done, {keepalive}, Rest};
        % 1 - Login
        ?PKT_LOGIN(PlayerID, UserName, Password) ->
            io:format("Pid: ~p~nUser Name: ~p~nPassword: ~p~n", [PlayerID, UserName, Password]),
            {done, {login, PlayerID, UserName, Password}, Rest};
        % 2 - Handshake
        ?PKT_HANDSHAKE(PlayerName) ->
            io:format("Player Name: ~p~n", [PlayerName]),
            {done, {handshake, PlayerName}, Rest};
        % 10 - Unknown, loaded?
        ?PKT_LOADED(Loaded) ->
            {done, {loaded, Loaded}, Rest};
        % 13 - Player move look
        ?PKT_PLAYER_MOVE_LOOK(X, Y, S, Z, R, P, U) ->
            io:format("Look: [~p,~p,~p]~n", [X,Y,Z]),
            {done, {player_move_look, X, Y, S, Z, R, P, U}, Rest};
        _ ->
            {more, Data}
    end.

handle_packet(_Client, {handshake, PlayerName}) ->
    io:format("C->S: Welcome: ~p~n", [PlayerName]),
    mc_reply:handshake(false);

handle_packet(_Client, {login, PlayerID, Username, Password}) ->
    io:format("C->S: PlayerID: ~p~nLogin: ~p~nPass: ~p~n", [PlayerID, Username, Password]),
    mc_reply:login(0, "", "");

handle_packet(Client, {player_move_look, X, Y, S, Z, R, P, U}) ->
    gen_server:cast(Client, {position, X, Y, Z, S, R, P}),

    io:format("C->S: PML: [~p,~p,~p] [~p,~p,~p] ~p~n", [X,Y,Z, S,R,P,U]),
    none;

handle_packet(_State, {loaded, _Loaded}) ->
    case _Loaded of
        0 -> ok;
        Value ->
            io:format("Loaded: ~p~n", [Value])
    end,
    none;
handle_packet(_State, {keepalive} ) ->
    mc_reply:keepalive();

handle_packet(_State, Unknown) ->
    io:format("Unknown Packet: ~p~n", [Unknown]),
    <<"">>.
