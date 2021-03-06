-module(mc_util).

-compile(export_all).

write_packet(PacketID, Contents) ->
    BinaryData = encode_list(Contents),
    <<PacketID:8, BinaryData/binary>>.

or_binaries(BinA, BinB) when size(BinA) =:= size(BinB) ->
    or_binaries(BinA, BinB, <<>>).

or_binaries(<<>>, <<>>, Buffer) ->
    Buffer;
or_binaries(BinA, BinB, Buffer) ->
    <<A:8/integer, RestA/binary>> = BinA,
    <<B:8/integer, RestB/binary>> = BinB,
    C = A bor B,
    NewBuffer = <<Buffer/binary, C:8/integer>>,
    or_binaries(RestA, RestB, NewBuffer).

expand_4_to_8(Bytes) ->
    expand_4_to_8(Bytes, <<>>).

expand_4_to_8(<<>>, Buffer) ->
    Buffer;
expand_4_to_8(Bytes, Buffer) ->
    <<Chunk:4/bits, Rest/bits>> = Bytes,
    ByteChunk = << <<0:4>>/bits, Chunk/bits>>,
    NewBuff = <<Buffer/bits, ByteChunk/bits>>,
    expand_4_to_8(Rest, NewBuff).

encode_list([], Acc) ->
    Acc;

encode_list([H|T], Acc) ->
    Value = encode_value(H),
    encode_list(T, <<Acc/binary, Value/bits>>).

encode_list(List) ->
    encode_list(List, <<>>).

encode_value({int, Int}) when is_integer(Int)->
    <<Int:32/integer-signed-big>>;
encode_value({bool, Bool}) when is_integer(Bool) ->
    <<Bool:8>>;
encode_value({long, Long}) when is_integer(Long) ->
    <<Long:64/integer-signed-big>>;
encode_value({short, Short}) when is_integer(Short) ->
    <<Short:16/integer-signed-big>>;
encode_value({byte, Byte}) when is_integer(Byte) ->
    <<Byte:8>>;
encode_value({nibble, Nibble}) when is_integer(Nibble) ->
    <<Nibble:4>>;
encode_value({double, Double}) when is_float(Double)->
    <<Double:64/float>>;
encode_value({double, Double}) when is_integer(Double) ->
    encode_value({double, float(Double)});
encode_value({float, Float}) when is_float(Float) ->
    <<Float:32/float>>;
encode_value({float, Float}) when is_integer(Float) ->
    encode_value({float, float(Float)});
encode_value({binary, Binary}) when is_binary(Binary) ->
    Binary;
encode_value({string, String}) when is_list(String) ->
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
            Light = case 128-Y of
                Dark when Dark >= 0, Dark =< 63 ->
                    0;
                _ ->
                    255
            end,
            case 128-Y of
                0 ->
                    %io:format("Bedrock: [~p, ~p, ~p]~n", [X, Y, Z]),
                    {block, 7, 0, Light};
                Ground when Ground >=0, Ground =< 63 ->
                    %io:format("Dirt: [~p, ~p, ~p]~n", [X, Y, Z]),
                    {block, 3,0,Light};
                Ground when Ground == 64 ->
                    %io:format("Grass: [~p, ~p, ~p]~n", [X, Y, Z]),
                    {block, 2,0,Light};
                _ ->
                    %io:format("Air: [~p, ~p, ~p]~n", [X, Y, Z]),
                    {block, 0,0,Light}
            end
        end, array:new(Blocks)),
    lists:flatten(array:foldl(
        fun(_Idx, Value, [Type,Meta,Light]) ->
            {block, BType, BMeta, BLight} = Value,
            [[{byte, BType}|Type], [{byte, BMeta}|Meta], [{nibble,BLight}|Light]]
    end, [[],[],[]], Array)).

