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
            Light = case Y of
                Dark when Dark >= 0, Dark =< 63 ->
                    0;
                _ ->
                    255
            end,
            case Y of
                0 -> {block, 7, 0, Light};
                Ground when Ground >=0, Ground =< 30 ->
                    {block, 3,0,Light};
                Ground when Ground == 31 ->
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

