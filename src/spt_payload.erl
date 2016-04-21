%%% ==================================================================
%%% @author:zhuoyikang
%%% @doc 基础类型协议解析
%%% @end
%%% ==================================================================

-module(spt_payload).
-compile(export_all).

encode_integer(Int) -> <<Int:32/signed-big-integer>>.
decode_integer(<<Integer:32/signed-big-integer, Data/binary>>) ->
    {Integer, Data}.


encode_short(Short) -> <<Short:16/signed-big-integer>>.
decode_short(<<Short:16/signed-big-integer, Data/binary>>) ->
    {Short, Data}.


encode_char(Short) -> <<Short:8/signed-big-integer>>.
decode_char(<<Short:8/signed-big-integer, Data/binary>>) ->
    {Short, Data}.


encode_uinteger(Short) -> <<Short:32/unsigned-big-integer>>.
decode_uinteger(<<Short:32/unsigned-big-integer, Data/binary>>) ->
    {Short, Data}.


encode_ushort(Short) -> <<Short:16/unsigned-big-integer>>.
decode_ushort(<<Short:16/unsigned-big-integer, Data/binary>>) ->
    {Short, Data}.


encode_uchar(Short) -> <<Short:8/unsigned-big-integer>>.
decode_uchar(<<Short:8/unsigned-big-integer, Data/binary>>) ->
    {Short, Data}.

%% 解析float
encode_float(Float) when is_float(Float) ->
    <<Float:32/float>>.
decode_float(<<Float:32/float, Data/binary>>)  ->
    {Float, Data}.

encode_string(<<BinS/binary>>) ->
    L = byte_size(BinS),
    list_to_binary([<<L:16>>, BinS]).
decode_string(<<Length:16/unsigned-big-integer,Data/binary>>) ->
    {StringData, StringLeftData} = split_binary(Data,Length),
    {StringData, StringLeftData}.


encode_boolean(Bool) when is_boolean(Bool) ->
    case Bool of
        true -> <<1:8>>;
        false -> <<0:8>>
    end.
decode_boolean(<<BoolVal:8, Data/binary>>) ->
    case BoolVal of
        0 -> {false, Data};
        _ -> {true, Data}
    end.
