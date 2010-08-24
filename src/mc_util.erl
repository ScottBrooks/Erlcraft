-module(mc_util).

-compile(export_all).

write_packet(PacketID, Contents) ->
    BinaryData = encode_list(Contents),
    <<PacketID:8, BinaryData/binary>>.


encode_list([], Acc) ->
    Acc;

encode_list([H|T], Acc) ->
    Value = encode_value(H),
    encode_list(T, <<Acc/binary, Value/bits>>).

encode_list(List) ->
    encode_list(List, <<>>).

encode_value({int, Int}) ->
    <<Int:32/integer-signed-big>>;
encode_value({bool, Bool}) ->
    <<Bool:8>>;
encode_value({long, Long}) ->
    <<Long:64/integer-signed-big>>;
encode_value({short, Short}) ->
    <<Short:16/integer-signed-big>>;
encode_value({byte, Byte}) ->
    <<Byte:8>>;
encode_value({nibble, Nibble}) ->
    <<Nibble:4>>;
encode_value({double, Double}) ->
    <<Double:64/float>>;
encode_value({float, Float}) ->
    <<Float:32/float>>;
encode_value({binary, Binary}) ->
    Binary;
encode_value({string, String}) ->
    Len = length(String),
    BinVal = list_to_binary(String),
    <<Len:16/integer-signed-big, BinVal/binary>>.


block_chunk()->
    [{byte,1}, {byte,1},{nibble,5}].

chunk_size(X,Y,Z)->
    Size = round(X * Y * Z * 2.5),
    io:format("Chunk Size: ~p~n", [Size]),
    Size.

chunk_data(Blocks) ->
    Array = array:map(fun(Idx,_Value) -> 
            Z = trunc(Idx/2048),
            X = trunc((Idx-Z*2048)/128),
            Y = Idx - Z*2048 - X*128,
            Light = case Y of
                Dark when Dark >= 0, Dark =< 63 ->
                    0;
                _ ->
                    255
            end,
            case Y of
                0 -> {block, 7, 0, Light};
                Ground when Ground >=0, Ground =< 63 ->
                    {block, 3,0,Light};
                Ground when Ground == 64 ->
                    {block, 2,0,Light};
                _ ->
                    {block, 0,0,Light}
            end
        end, array:new(Blocks)),
    lists:flatten(array:foldl(
        fun(_Idx, Value, [Type,Meta,Light]) ->
            {block, BType, BMeta, BLight} = Value,
            [[{byte, BType}|Type], [{byte, BMeta}|Meta], [{nibble,BLight}|Light]]
    end, [[],[],[]], Array)).

generate_pre_chunk(X,Z, Update) ->
    write_packet(16#32, [{int, X}, {int, Z}, {bool, Update}]).

generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ) ->
    Data = chunk_data(SizeX * SizeY * SizeX),
    Compressed = zlib:zip(encode_list(Data)),

    write_packet(16#33, lists:flatten([{int, X*16}, {short, Y}, {int, Z*16}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])).

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
        lists:seq(-31,31)),
    Pos = mc_reply:position_and_look(0, 100, 0, 0, 0, 0),
    gen_fsm:send_all_state_event(Pid, {packet, Pos}),
    Time = mc_reply:timestamp(1000),
    gen_fsm:send_all_state_event(Pid, {packet, Time}),
    ok.
