-module(mc).

-export([handle_packet/2, handle_data/1, test_stream/1]).

% Packet macros
% If we have multiple strings in one packet, we need to use the other PKT_STRING2,N macros
-define(PKT_STRING(StrName), _Len:16/integer-signed-big, StrName:_Len/binary).
-define(PKT_STRING2(StrName2), _Len2:16/integer-signed-big, StrName2:_Len2/binary).
-define(PKT_STRING3(StrName3), _Len3:16/integer-signed-big, StrName3:_Len3/binary).
-define(PKT_SHORT(ShortValue), ShortValue:16/integer-signed-big).
-define(PKT_INTEGER(IntValue), IntValue:32/integer-signed-big).
-define(PKT_LONG(IntValue), IntValue:64/integer-signed-big).
-define(PKT_FLOAT(FloatValue), FloatValue:32/float).
-define(PKT_FLOAT2(FloatValue2), FloatValue2:32/float).
-define(PKT_DOUBLE(DoubleValue), DoubleValue:64/float).
-define(PKT_BOOL(BoolValue), BoolValue:8).
-define(PKT_BYTE(ByteValue), ByteValue:8).
-define(PKT_BINARY(Binary), _BinLen:32/integer-signed-big, Binary:_BinLen/binary).

% Client -> Server

-define(PKT_KEEPALIVE,                           <<16#00, Rest/binary>>).
-define(PKT_LOGIN(PlayerID, N, P),               <<16#01, ?PKT_INTEGER(PlayerID), ?PKT_STRING(N), ?PKT_STRING2(P),Rest/binary>>).
-define(PKT_HANDSHAKE(PlayerName),               <<16#02, ?PKT_STRING(PlayerName), Rest/binary>>).
-define(PKT_CHAT(Message),                       <<16#03, ?PKT_STRING(Message), Rest/binary>>).
-define(PKT_UPDATE_TIME(Time),                   <<16#04, ?PKT_LONG(Time), Rest/binary>>).
%%% Damn you notch, just had to make this one a pain to parse
-define(PKT_PLAYER_INVENTORY(T, C),              <<16#05, ?PKT_INTEGER(T), ?PKT_SHORT(C), More/binary>>).
-define(PKT_COMPASS(X,Y,Z),                      <<16#06, ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), Rest/binary>>).
-define(PKT_FLYING(Flying),                      <<16#0A, ?PKT_BOOL(Flying), Rest/binary>>).
-define(PKT_PLAYER_POSITION(X,Y,Z,S,U),          <<16#0B, ?PKT_DOUBLE(X), ?PKT_DOUBLE(Y), ?PKT_DOUBLE(S), ?PKT_DOUBLE(Z), ?PKT_BOOL(U), Rest/binary>>).
-define(PKT_PLAYER_LOOK(R,P,U),                  <<16#0C, ?PKT_FLOAT(R), ?PKT_FLOAT(P), ?PKT_BOOL(U), Rest/binary>>).
-define(PKT_PLAYER_MOVE_LOOK(X,Y,Z,S,R,P,U),     <<16#0D, ?PKT_DOUBLE(X), ?PKT_DOUBLE(Y), ?PKT_DOUBLE(S), ?PKT_DOUBLE(Z), ?PKT_FLOAT(R), ?PKT_FLOAT(P), ?PKT_BOOL(U), Rest/binary>>).
-define(PKT_BLOCK_DIG(S, X, Y, Z, U),            <<16#0E, ?PKT_BYTE(S), ?PKT_INTEGER(X), ?PKT_BYTE(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(U), Rest/binary>>).
-define(PKT_PLACE(T, X, Y, Z, D),                <<16#0F, ?PKT_SHORT(T), ?PKT_INTEGER(X), ?PKT_BYTE(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(D), Rest/binary>>).
-define(PKT_BLOCK_SWITCH(E, I),                  <<16#10, ?PKT_INTEGER(E), ?PKT_SHORT(I), Rest/binary>>).
-define(PKT_ADD_TO_INVENTORY(T, A, L),           <<16#11, ?PKT_SHORT(T), ?PKT_BYTE(A), ?PKT_SHORT(L), Rest/binary>>).
-define(PKT_ARM_ANIMATION(E, U),                 <<16#12, ?PKT_INTEGER(E), ?PKT_BYTE(U), Rest/binary>>).
-define(PKT_NAMED_ENTITY_SPAWN(ID,N,X,Y,Z,R,P,U),<<16#14, ?PKT_INTEGER(ID), ?PKT_STRING(N), ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(R), ?PKT_BYTE(P), ?PKT_SHORT(U), Rest/binary>>).
-define(PKT_PICKUP_SPAWN(ID,I,U1,X,Y,Z,R,P,U2),  <<16#15, ?PKT_INTEGER(ID), ?PKT_SHORT(I), ?PKT_BYTE(U1), ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(R), ?PKT_BYTE(P), ?PKT_BYTE(U2), Rest/binary>>).
-define(PKT_COLLECT_ITEM(Collected, Collector),  <<16#16, ?PKT_INTEGER(Collected), ?PKT_INTEGER(Collector), Rest/binary>>).

-define(PKT_ADD_OBJECT(ID,T,X,Y,Z),              <<16#17, ?PKT_INTEGER(ID), ?PKT_BYTE(T), ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), Rest/binary>>).
-define(PKT_MOB_SPAWN(ID,T,X,Y,Z,R,P),           <<16#18, ?PKT_INTEGER(ID), ?PKT_BYTE(T), ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(R), ?PKT_BYTE(P), Rest/binary>>).
-define(PKT_DESTROY_ENTITY(ID),                  <<16#1D, ?PKT_INTEGER(ID), Rest/binary>>).
-define(PKT_ENTITY(ID),                          <<16#1E, ?PKT_INTEGER(ID), Rest/binary>>).
-define(PKT_REL_ENTITY_MOVE(ID,X,Y,Z),           <<16#1F, ?PKT_INTEGER(ID), ?PKT_BYTE(X), ?PKT_BYTE(Y), ?PKT_BYTE(Z), Rest/binary>>).
-define(PKT_ENTITY_LOOK(ID, R, P),               <<16#20, ?PKT_INTEGER(ID), ?PKT_BYTE(R), ?PKT_BYTE(P), Rest/binary>>).
-define(PKT_REL_ENTITY_MOVE_LOOK(ID,X,Y,Z,R,P),  <<16#21, ?PKT_INTEGER(ID), ?PKT_BYTE(X), ?PKT_BYTE(Y), ?PKT_BYTE(Z), ?PKT_BYTE(R), ?PKT_BYTE(P), Rest/binary>>).
-define(PKT_ENTITY_TELEPORT(ID,X,Y,Z,R,P),       <<16#22, ?PKT_INTEGER(ID), ?PKT_INTEGER(X), ?PKT_INTEGER(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(R), ?PKT_BYTE(P), Rest/binary>>).
-define(PKT_PRE_CHUNK(X,Z,M),                    <<16#32, ?PKT_INTEGER(X), ?PKT_INTEGER(Z), ?PKT_INTEGER(M), Rest/binary>>).
-define(PKT_MAP_CHUNK(X,Y,Z,SX,SY,SZ,CC),        <<16#33, ?PKT_INTEGER(X), ?PKT_SHORT(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(SX), ?PKT_BYTE(SY), ?PKT_BYTE(SZ), ?PKT_BINARY(CC), Rest/binary>>).
-define(PKT_MULTI_BLOCK_CHANGE(X,Z,CA,TA,MA),    <<16#34, ?PKT_INTEGER(X), ?PKT_INTEGER(Z), _AS:16/integer-signed-big, CA:_AS/binary, TA:_AS/binary, MA:_AS/binary, Rest/binary>>).
-define(PKT_BLOCK_CHANGE(X,Y,Z,T,M),             <<16#35, ?PKT_INTEGER(X), ?PKT_BYTE(Y), ?PKT_INTEGER(Z), ?PKT_BYTE(T), ?PKT_BYTE(M), Rest/binary>>).
-define(PKT_COMPLEX_ENTITY(X,Y,Z,Payload),       <<16#3B, ?PKT_INTEGER(X), ?PKT_SHORT(Y), ?PKT_INTEGER(Z), _PS:16/integer-signed-big, Payload:_PS/binary, Rest/binary>>).
-define(PKT_KICK(Message),                       <<16#FF, ?PKT_STRING(Message), Rest/binary>>).


handle_data(Data) ->
    case Data of
        % 0x00 - Keep Alive
        ?PKT_KEEPALIVE -> {done, {keepalive}, Rest};
        % 0x01 - Login
        ?PKT_LOGIN(PlayerID, UserName, Password) ->
            {done, {login, PlayerID, UserName, Password}, Rest};
        % 0x02 - Handshake
        ?PKT_HANDSHAKE(PlayerName) ->
            {done, {handshake, PlayerName}, Rest};
        % 0x03 - Chat
        ?PKT_CHAT(Message) ->
            {done, {chat, Message}, Rest};
        % 0x04 - Time
        ?PKT_UPDATE_TIME(Time) ->
            {done, {update, Time}, Rest};
        % 0x05 - Inventory
        ?PKT_PLAYER_INVENTORY(Type, Count) ->
            {Data, Rest} = parse_inventory(Count, More),
            {done, {inventory, Type, Data}, Rest};
        % 0x06 - Spawn(Compass location)
        ?PKT_COMPASS(X, Y, Z) ->
            {done, {compass, X, Y, Z}, Rest};
        % 0x0A - Flying
        ?PKT_FLYING(Flying) ->
            {done, {flying, Flying}, Rest};
        % 0x0B - Player position
        ?PKT_PLAYER_POSITION(X,Y,Z,S,U) ->
            {done, {player_position, X,Y,Z,S,U}, Rest};
        % 0x0C - Player look
        ?PKT_PLAYER_LOOK(R,P,U) ->
            {done, {player_look, R,P,U}, Rest};
        % 0x0D - Player move look
        ?PKT_PLAYER_MOVE_LOOK(X, Y, Z, S, R, P, U) ->
            {done, {player_move_look, X, Y, Z, S, R, P, U}, Rest};
        % 0x0E - Block Dig
        ?PKT_BLOCK_DIG(Status, X, Y, Z, U) ->
            {done, {block_dig, Status, X, Y, Z, U}, Rest};
        % 0x0F - Place
        ?PKT_PLACE(Type, X, Y, Z, Direction) ->
            {done, {place, Type, X, Y, Z, Direction}, Rest};
        % 0x10 - Block/Item Switch
        ?PKT_BLOCK_SWITCH(EntityID, ItemCode) ->
            {done, {block_switch, EntityID, ItemCode}, Rest};
        % 0x11 - Add to Inventory
        ?PKT_ADD_TO_INVENTORY(Type, Amount, Life) ->
            {done, {add_to_inventory, Type, Amount, Life}, Rest};
        % 0x12 - Arm Animation
        ?PKT_ARM_ANIMATION(EntityID, U) ->
            {done, {arm_animation, EntityID, U}, Rest};
        % 0x14 - Named Entity Spawn
        ?PKT_NAMED_ENTITY_SPAWN(ID, Name, X, Y, Z, Rotation, Pitch, Item) ->
            {done, {named_entity_spawn, ID, Name, X, Y, Z, Rotation, Pitch, Item}, Rest};
        % 0x15 - Pickup Spawn
        ?PKT_PICKUP_SPAWN(ID, Item, U1, X, Y, Z, R, P, U2) ->
            {done, {pickup_spawn, ID, Item, U1, X, Y, Z, R, P, U2}, Rest};
        % 0x16 - Collect Item
        ?PKT_COLLECT_ITEM(Collected, Collector) ->
            {done, {collect, Collected, Collector}, Rest};
        % 0x17 - Add object/vehicle
        ?PKT_ADD_OBJECT(ID, Type, X, Y, Z) ->
            {done, {add_object, ID, Type, X, Y, Z}, Rest};
        % 0x18 - Mob Spawn
        ?PKT_MOB_SPAWN(ID, Type, X, Y, Z, R, P) ->
            {done, {mob_spawn, ID, Type, X, Y, Z, R, P}, Rest};
        % 0x1D - Destroy Entity
        ?PKT_DESTROY_ENTITY(ID) ->
            {done, {destroy_entity, ID}, Rest};
        % 0x1E - Entity
        ?PKT_ENTITY(ID) ->
            {done, {entity, ID}, Rest};
        % 0x1F - Relative Entity Move
        ?PKT_REL_ENTITY_MOVE(ID, X, Y, Z) ->
            {done, {rel_entity_move, ID, X, Y, Z}, Rest};
        % 0x20 - Entity Look
        ?PKT_ENTITY_LOOK(ID, R, P) ->
            {done, {entity_look, ID, R, P}, Rest};
        % 0x21 - Relative Entity Move Look
        ?PKT_REL_ENTITY_MOVE_LOOK(ID, X, Y, Z, R, P) ->
            {done, {rel_entity_move_look, ID, X, Y, Z, R, P}, Rest};
        % 0x22 - Entity Teleport
        ?PKT_ENTITY_TELEPORT(ID, X, Y, Z, R, P) ->
            {done, {entity_teleport, ID, X, Y, Z, R, P}, Rest};
        % 0x32 - Pre Chunk
        ?PKT_PRE_CHUNK(X, Z, Mode) ->
            {done, {pre_chunk, X, Z, Mode}, Rest};
        % 0x33 - Map Chunk
        ?PKT_MAP_CHUNK(X, Y, Z, SizeX, SizeY, SizeZ, Chunk) ->
            {done, {map_chunk, X, Y, Z, SizeX, SizeY, SizeZ, Chunk}, Rest};
        % 0x34 - Multi Block Change
        ?PKT_MULTI_BLOCK_CHANGE(X, Z, CoordArray, TypeArray, MetaArray) ->
            {done, {multi_block_change, X, Z, CoordArray, TypeArray, MetaArray}, Rest};
        % 0x35 - Block Change
        ?PKT_BLOCK_CHANGE(X, Y, Z, Type, Meta) ->
            {done, {block_change, X, Y, Z, Type, Meta}, Rest};
        % 0x3B - Complex Entity
        ?PKT_COMPLEX_ENTITY(X, Y, Z, Payload) ->
            {done, {complex_entity, X, Y, Z, Payload}, Rest};
        % 0xFF - Kick
        ?PKT_KICK(Message) ->
            {done, {kick, Message}, Rest};
        _ ->
            %io:format("More: [~p]~n", [Data]),
            {more, Data}
    end.


parse_inventory(Count, Data) ->
    parse_inventory(Count, Data, []).

parse_inventory(Count, Data, Items) ->
    case Data of
        <<-1/integer-signed-big, MoreData/binary>> ->
            parse_inventory(Count-1, MoreData, [{empty}|Items]);
        <<?PKT_SHORT(ItemID), ?PKT_BYTE(Count), ?PKT_SHORT(Health), MoreData/binary>> ->
            parse_inventory(Count-1, MoreData, [{item, ItemID, Count, Health}|Items])
    end.


handle_packet(_Client, {handshake, _PlayerName}) ->
%    io:format("C->S: Welcome: ~p~n", [PlayerName]),
    mc_reply:handshake(false);

handle_packet(Client, {login, PlayerID, _Username, _Password}) ->
%    io:format("C->S: PlayerID: ~p~nLogin: ~p~nPass: ~p~n", [PlayerID, Username, Password]),
    gen_server:cast(Client, {client_begin, PlayerID}),
    mc_reply:login(PlayerID, "", "");

handle_packet(Client, {player_move_look, X, Y, Z, S, R, P, _U}) ->
    gen_server:cast(Client, {move_look, X, Y, Z, S, R, P}),

%    io:format("C->S: PML: [~p,~p,~p] [~p,~p,~p] ~p~n", [X,Y,Z, S,R,P,U]),
    none;

handle_packet(Client, {player_look, R, P, U}) ->
    gen_server:cast(Client, {look, R, P, U}),
%    io:format("C->S: L: [~p,~p,~p]~n", [R,P,U]),
    none;

handle_packet(Client, {player_position, X, Y, Z, S, U}) ->
    gen_server:cast(Client, {position, X, Y, Z, S, U}),
%    io:format("C->S: P: [~p,~p,~p,~p]~p~n", [X, Y, Z, S, U]),
    none;

handle_packet(Client, {flying, Flying}) ->
    gen_server:cast(Client, {flying, Flying}),
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

handle_packet(Client, {kick, Message}) ->
    gen_server:cast(Client, {kick, Message}),
    none;

handle_packet(_Client, {arm_animation, _EntityID, _Unknown}) ->
    none;

handle_packet(Client, {block_dig, Stage, X, Y, Z, Direction}) ->
    case mc_world:block_dig(Stage, X, Y, Z, Direction, Client) of
        {block_change, X, Y, Z, Type, Meta} ->
            mc_reply:block_change(X, Y, Z, Type, Meta);
        _ ->
            none
    end;

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

