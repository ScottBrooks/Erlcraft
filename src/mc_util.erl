-module(mc_util).

-compile(export_all).

write_packet(PacketID, Contents) ->
    BinaryData = encode_list(Contents),
    LeftOver = bit_size(BinaryData) rem 8,
    <<PacketID:8, BinaryData/bits, <<0:LeftOver>>/bits>>.


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
    lists:flatten(lists:foldl(fun(_X, [Type,Meta,Light]) -> [[{byte, 1}|Type], [{byte,0}|Meta], [{nibble,5}|Light]] end, [[],[],[]], lists:seq(1,Blocks))).


generate_chunk(X, Y, Z, SizeX, SizeY, SizeZ, Update) ->
    PreChunk = write_packet(32, [{int, X}, {int, Z}, {bool, Update}]),
    Data = chunk_data(SizeX * SizeY * SizeX),
    Compressed = zlib:compress(encode_list(Data)),

    Chunks = write_packet(33, lists:flatten([{int, X}, {short, Y}, {int, Z}, {byte, SizeX-1}, {byte, SizeY-1}, {byte, SizeZ-1}, {int, size(Compressed)}, {binary, Compressed}])),
    <<PreChunk/binary, Chunks/binary>>.

fake_world() ->
    Data1 = generate_chunk(128,0,-192, 16, 128, 16, 1),
    Data2 = generate_chunk(127,0,-192, 16, 128, 16, 1),
    Data3 = generate_chunk(129,0,-192, 16, 128, 16, 1),

    <<Data1/binary,Data2/binary,Data3/binary>>.
