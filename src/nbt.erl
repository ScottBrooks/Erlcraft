-module(nbt).


-export([load_file/1]).

-define(NBT_STRING(StrName), _Len:16/integer-signed-big, StrName:_Len/binary).
-define(NBT_STRING2(StrName2), _Len2:16/integer-signed-big, StrName2:_Len2/binary).
-define(NBT_SHORT(ShortValue), ShortValue:16/integer-signed-big).
-define(NBT_INTEGER(IntValue), IntValue:32/integer-signed-big).
-define(NBT_LONG(IntValue), IntValue:64/integer-signed-big).
-define(NBT_FLOAT(FloatValue), FloatValue:32/float).
-define(NBT_DOUBLE(DoubleValue), DoubleValue:64/float).
-define(NBT_BOOL(BoolValue), BoolValue:8).
-define(NBT_BYTE(ByteValue), ByteValue:8).
-define(NBT_BINARY(Binary), _BinLen:32/integer-signed-big, Binary:_BinLen/binary).

-define(TAG_END, <<0, Rest/binary>>).
-define(TAG_Byte(Name, Payload),       <<1, ?NBT_STRING(Name), ?NBT_BYTE(Payload), Rest/binary>>).
-define(TAG_Short(Name, Payload),      <<2, ?NBT_STRING(Name), ?NBT_SHORT(Payload), Rest/binary>>).
-define(TAG_Int(Name, Payload),        <<3, ?NBT_STRING(Name), ?NBT_INTEGER(Payload), Rest/binary>>).
-define(TAG_Long(Name, Payload),       <<4, ?NBT_STRING(Name), ?NBT_LONG(Payload), Rest/binary>>).
-define(TAG_Float(Name, Payload),      <<5, ?NBT_STRING(Name), ?NBT_FLOAT(Payload), Rest/binary>>).
-define(TAG_Double(Name, Payload),     <<6, ?NBT_STRING(Name), ?NBT_DOUBLE(Payload), Rest/binary>>).
-define(TAG_Byte_Array(Name, Payload), <<7, ?NBT_STRING(Name), ?NBT_BINARY(Payload), Rest/binary>>).
-define(TAG_String(Name, Payload),     <<8, ?NBT_STRING(Name), ?NBT_STRING2(Payload), Rest/binary>>).
-define(TAG_List(Name, Type, Length),  <<9, ?NBT_STRING(Name), ?NBT_BYTE(Type), ?NBT_INTEGER(Length), Rest/binary>>).
-define(TAG_Compound(Name),            <<10,?NBT_STRING(Name), Rest/binary>>).


load_file(Path) ->
    {ok, CompressedData} = file:read_file(Path),
    Data = zlib:gunzip(CompressedData),
    Parsed = parse_nbt(Data),
    io:format("Parsed Data: ~p~n", [Parsed]).

parse_nbt(Data) ->
    parse_nbt(Data, []).

parse_nbt(Data, Buffer) ->
    case parse_nbt_stream(Data) of
        {done, DecodedPacket, <<>>} ->
            io:format("Buffer End:~n", []),
            {<<>>, [DecodedPacket|Buffer]};
        {done, DecodedPacket, Rest} ->
            io:format("DP: ~pB: ~p~n", [DecodedPacket, Buffer]),
            case DecodedPacket of
                {tag_end} -> {Rest, Buffer};
                {tag_list, Name, Type, Length} ->
                    {ChildRest, ChildBuffer} = parse_nbt_list(Rest, Type, Length, []),
                    parse_nbt(ChildRest, [{tag_list, Name, Type, lists:reverse(ChildBuffer)}|Buffer]);
                {tag_compound, Name} ->
                    {ChildRest, ChildBuffer} = parse_nbt(Rest, []),
                    parse_nbt(ChildRest, [{tag_compound, Name, lists:reverse(ChildBuffer)}|Buffer]);
                _ ->
                    parse_nbt(Rest, [DecodedPacket|Buffer])
            end
    end.


parse_nbt_stream(Data) ->
    case Data of
        ?TAG_END                      -> {done, {tag_end}, Rest};
        ?TAG_Byte(Name, Value)        -> {done, {tag_byte, Name, Value}, Rest};
        ?TAG_Short(Name, Value)       -> {done, {tag_short, Name, Value}, Rest};
        ?TAG_Int(Name, Value)         -> {done, {tag_int, Name, Value}, Rest};
        ?TAG_Long(Name, Value)        -> {done, {tag_long, Name, Value}, Rest};
        ?TAG_Float(Name, Value)       -> {done, {tag_float, Name, Value}, Rest};
        ?TAG_Double(Name, Value)      -> {done, {tag_double, Name, Value}, Rest};
        ?TAG_Byte_Array(Name, Value)  -> {done, {tag_byte_array, Name, Value}, Rest};
        ?TAG_String(Name, Value)      -> {done, {tag_string, Name, Value}, Rest};
        ?TAG_List(Name, Type, Length) -> {done, {tag_list, Name, Type, Length}, Rest};
        ?TAG_Compound(Name)           -> {done, {tag_compound, Name}, Rest};
        <<>>                          -> {done, {eof}, <<>>};
        _ -> io:format("Unknown: ~p~n", [Data]),
            {done, {tag_unknown, Data}, <<>>}
    end.

parse_nbt_list(Data, _Type, 0, Buffer) ->
    {Data, Buffer};
parse_nbt_list(Data, Type, Length, Buffer) ->
    {Item, DataRest} = case Type of
        1 ->
            <<?NBT_BYTE(Value), Rest/binary>> = Data,
            {Value, Rest};
        2 ->
            <<?NBT_SHORT(Value), Rest/binary>> = Data,
            {Value, Rest};
        3 ->
            <<?NBT_INTEGER(Value), Rest/binary>> = Data,
            {Value, Rest};
        4 ->
            <<?NBT_LONG(Value), Rest/binary>> = Data,
            {Value, Rest};
        5 ->
            <<?NBT_FLOAT(Value), Rest/binary>> = Data,
            {Value, Rest};
        6 ->
            <<?NBT_DOUBLE(Value), Rest/binary>> = Data,
            {Value, Rest};
        7 ->
            <<?NBT_INTEGER(Len), Bytes:Len/binary, Rest/binary>> = Data,
            {Bytes, Rest};
        8 ->
            <<?NBT_STRING(Value), Rest/binary>> = Data,
            {Value, Rest};
        9 ->
            <<?NBT_SHORT(Value), Rest/binary>> = Data,
            {Value, Rest};
        10 ->
            {ChildRest, ChildBuffer} = parse_nbt(Data, []), {ChildBuffer, ChildRest}
    end,

    parse_nbt_list(DataRest, Type, Length-1, [Item|Buffer]).

