-module(mc).

-export([handle_packet/2, handle_data/1, test_stream/1]).

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

-define(PKT_KEEPALIVE,                       <<16#0, Rest/binary>>).
-define(PKT_LOGIN(PlayerID, N, P),           <<16#1, ?PKT_INTEGER(PlayerID), ?PKT_STRING(N), ?PKT_STRING2(P),Rest/binary>>).
-define(PKT_HANDSHAKE(PlayerName),           <<16#2, ?PKT_STRING(PlayerName), Rest/binary>>).
-define(PKT_LOADED(Loaded),                  <<16#0A, ?PKT_BOOL(Loaded), Rest/binary>>).
-define(PKT_PLAYER_POSITION(X,Y,Z,S,U),      <<16#0B, ?PKT_DOUBLE(X), ?PKT_DOUBLE(Y), ?PKT_DOUBLE(S), ?PKT_DOUBLE(Z), ?PKT_BOOL(U), Rest/binary>>).
-define(PKT_PLAYER_LOOK(R,P,U),              <<16#0C, ?PKT_FLOAT(R), ?PKT_FLOAT(P), ?PKT_FLOAT(U), Rest/binary>>).
-define(PKT_PLAYER_MOVE_LOOK(X,Y,Z,S,R,P,U), <<16#0D, ?PKT_DOUBLE(X), ?PKT_DOUBLE(Y), ?PKT_DOUBLE(S), ?PKT_DOUBLE(Z), ?PKT_FLOAT(R), ?PKT_FLOAT(P), ?PKT_BOOL(U), Rest/binary>>).

handle_data(Data) ->
    case Data of
        % 0x00 - Keep Alive
        ?PKT_KEEPALIVE -> {done, {keepalive}, Rest};
        % 0x01 - Login
        ?PKT_LOGIN(PlayerID, UserName, Password) ->
            io:format("Pid: ~p~nUser Name: ~p~nPassword: ~p~n", [PlayerID, UserName, Password]),
            {done, {login, PlayerID, UserName, Password}, Rest};
        % 0x02 - Handshake
        ?PKT_HANDSHAKE(PlayerName) ->
            io:format("Player Name: ~p~n", [PlayerName]),
            {done, {handshake, PlayerName}, Rest};
        % 0x0A - Unknown, loaded?
        ?PKT_LOADED(Loaded) ->
            {done, {loaded, Loaded}, Rest};
        % 0x0B - Player position
        ?PKT_PLAYER_POSITION(X,Y,Z,S,U) ->
            {done, {player_position, X,Y,Z,S,U}, Rest};
        % 0x0C - Player look
        ?PKT_PLAYER_LOOK(R,P,U) ->
            {done, {player_look, R,P,U}, Rest};
        % 0x0D - Player move look
        ?PKT_PLAYER_MOVE_LOOK(X, Y, Z, S, R, P, U) ->
            io:format("Look: [~p,~p,~p]~n", [X,Y,Z]),
            {done, {player_move_look, X, Y, Z, S, R, P, U}, Rest};
        _ ->
            io:format("More: [~p]~n", [Data]),
            {more, Data}
    end.

handle_packet(_Client, {handshake, PlayerName}) ->
    io:format("C->S: Welcome: ~p~n", [PlayerName]),
    mc_reply:handshake(false);

handle_packet(Client, {login, PlayerID, Username, Password}) ->
    io:format("C->S: PlayerID: ~p~nLogin: ~p~nPass: ~p~n", [PlayerID, Username, Password]),
    gen_server:cast(Client, {player_id, PlayerID}),
    mc_reply:login(PlayerID, "", "");

handle_packet(Client, {player_move_look, X, Y, Z, S, R, P, U}) ->
    gen_server:cast(Client, {move_look, X, Y, Z, S, R, P}),

    io:format("C->S: PML: [~p,~p,~p] [~p,~p,~p] ~p~n", [X,Y,Z, S,R,P,U]),
    none;

handle_packet(Client, {player_look, R, P, U}) ->
    gen_server:cast(Client, {look, R, P, U}),
    io:format("C->S: L: [~p,~p,~p]~n", [R,P,U]),
    none;

handle_packet(Client, {player_position, X, Y, Z, S, U}) ->
    gen_server:cast(Client, {position, X, Y, Z, S, U}),
    io:format("C->S: P: [~p,~p,~p,~p]~p~n", [X, Y, Z, S, U]),
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

test_stream(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    test_data(Data).

test_data(Data) ->
    case mc:handle_data(Data) of
        {more, PartialData} ->
            <<Byte:8/integer, _Rest/binary>> = PartialData,
            io:format("Unknown packet: ~p~n", [Byte]);
        {done, DecodedPacket, Rest} ->
            case mc:handle_packet(none, DecodedPacket) of
                none -> io:format("No Reply: ~n", []);
                Reply -> io:format("Reply: [~p]~n", [Reply])
            end,
            test_data(Rest)
    end.

