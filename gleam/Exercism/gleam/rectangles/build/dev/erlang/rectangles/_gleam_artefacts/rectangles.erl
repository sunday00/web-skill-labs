-module(rectangles).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/rectangles.gleam").
-export([rectangles/1, main/0]).

-file("src/rectangles.gleam", 56).
-spec get_cur_cnt(
    integer(),
    {integer(), integer()},
    integer(),
    integer(),
    list({integer(), integer()}),
    integer(),
    integer()
) -> integer().
get_cur_cnt(Acc, Cur, Nc, Nl, Map, Max_l, Max_c) ->
    case (erlang:element(1, Cur) + Nl) >= Max_l of
        true ->
            Acc;

        false ->
            New_acc = case {begin
                    _pipe = Map,
                    gleam@list:contains(
                        _pipe,
                        {erlang:element(1, Cur), erlang:element(2, Cur) + Nc}
                    )
                end,
                begin
                    _pipe@1 = Map,
                    gleam@list:contains(
                        _pipe@1,
                        {erlang:element(1, Cur) + Nl, erlang:element(2, Cur)}
                    )
                end,
                begin
                    _pipe@2 = Map,
                    gleam@list:contains(
                        _pipe@2,
                        {erlang:element(1, Cur) + Nl,
                            erlang:element(2, Cur) + Nc}
                    )
                end} of
                {true, true, true} ->
                    Acc + 1;

                {_, _, _} ->
                    Acc
            end,
            case (erlang:element(2, Cur) + Nc) >= Max_c of
                true ->
                    get_cur_cnt(New_acc, Cur, 1, Nl + 1, Map, Max_l, Max_c);

                false ->
                    get_cur_cnt(New_acc, Cur, Nc + 1, Nl, Map, Max_l, Max_c)
            end
    end.

-file("src/rectangles.gleam", 45).
-spec reducer(integer(), list({integer(), integer()}), integer(), integer()) -> integer().
reducer(Acc, Map, Max_l, Max_c) ->
    case Map of
        [] ->
            Acc;

        [F | R] ->
            Ac = get_cur_cnt(0, F, 1, 1, Map, Max_l, Max_c),
            reducer(Acc + Ac, R, Max_l, Max_c)
    end.

-file("src/rectangles.gleam", 5).
-spec rectangles(binary()) -> integer().
rectangles(Input) ->
    case begin
        _pipe = Input,
        _pipe@1 = gleam@string:to_graphemes(_pipe),
        _pipe@2 = gleam@list:filter(_pipe@1, fun(El) -> El =:= <<"+"/utf8>> end),
        erlang:length(_pipe@2)
    end
    < 4 of
        true ->
            0;

        false ->
            Lines = begin
                _pipe@3 = Input,
                gleam@string:split(_pipe@3, <<"\n"/utf8>>)
            end,
            case begin
                _pipe@4 = Lines,
                erlang:length(_pipe@4)
            end
            < 2 of
                true ->
                    0;

                false ->
                    Map = begin
                        _pipe@5 = Lines,
                        _pipe@6 = gleam@list:drop(_pipe@5, 1),
                        gleam@list:index_fold(
                            _pipe@6,
                            [],
                            fun(Acc, L, I) -> _pipe@7 = L,
                                _pipe@8 = gleam@string:to_graphemes(_pipe@7),
                                gleam@list:index_fold(
                                    _pipe@8,
                                    Acc,
                                    fun(Ac, C, Ii) -> case C =:= <<"+"/utf8>> of
                                            true ->
                                                _pipe@9 = Ac,
                                                lists:append(_pipe@9, [{I, Ii}]);

                                            false ->
                                                Ac
                                        end end
                                ) end
                        )
                    end,
                    Max_c = begin
                        _pipe@10 = Lines,
                        gleam@list:fold(
                            _pipe@10,
                            0,
                            fun(Acc@1, Cur) ->
                                gleam@int:max(
                                    Acc@1,
                                    begin
                                        _pipe@11 = Cur,
                                        string:length(_pipe@11)
                                    end
                                )
                            end
                        )
                    end,
                    reducer(
                        0,
                        Map,
                        begin
                            _pipe@12 = Lines,
                            erlang:length(_pipe@12)
                        end,
                        Max_c
                    )
            end
    end.

-file("src/rectangles.gleam", 89).
-spec main() -> integer().
main() ->
    echo(
        rectangles(
            <<"
      +-+
      | |
    +-+-+
    | | |
    +-+-+
    "/utf8>>
        ),
        90
    ).

-define(is_lowercase_char(X), (X > 96 andalso X < 123)).
-define(is_underscore_char(X), (X == 95)).
-define(is_digit_char(X), (X > 47 andalso X < 58)).
-define(could_be_record(Tuple),
    erlang:is_tuple(Tuple) andalso
        erlang:is_atom(erlang:element(1, Tuple)) andalso
        erlang:element(1, Tuple) =/= false andalso
        erlang:element(1, Tuple) =/= true andalso
        erlang:element(1, Tuple) =/= nil
).
-define(is_atom_char(C),
    (?is_lowercase_char(C) orelse
        ?is_underscore_char(C) orelse
        ?is_digit_char(C))
).

-define(grey, "\e[90m").
-define(reset_color, "\e[39m").

echo(Value, Line) ->
    StringLine = erlang:integer_to_list(Line),
    StringValue = echo@inspect(Value),
    io:put_chars(
      standard_error,
      [?grey, ?FILEPATH, $:, StringLine, ?reset_color, $\n, StringValue, $\n]
    ),
    Value.

echo@inspect(Value) ->
    case Value of
        nil -> "Nil";
        true -> "True";
        false -> "False";
        Int when erlang:is_integer(Int) -> erlang:integer_to_list(Int);
        Float when erlang:is_float(Float) -> io_lib_format:fwrite_g(Float);
        Binary when erlang:is_binary(Binary) -> inspect@binary(Binary);
        Bits when erlang:is_bitstring(Bits) -> inspect@bit_array(Bits);
        Atom when erlang:is_atom(Atom) -> inspect@atom(Atom);
        List when erlang:is_list(List) -> inspect@list(List);
        Map when erlang:is_map(Map) -> inspect@map(Map);
        Record when ?could_be_record(Record) -> inspect@record(Record);
        Tuple when erlang:is_tuple(Tuple) -> inspect@tuple(Tuple);
        Function when erlang:is_function(Function) -> inspect@function(Function);
        Any -> ["//erl(", io_lib:format("~p", [Any]), ")"]
    end.

inspect@bit_array(Bits) ->
    Pieces = inspect@bit_array_pieces(Bits, []),
    Inner = lists:join(", ", lists:reverse(Pieces)),
    ["<<", Inner, ">>"].

inspect@bit_array_pieces(Bits, Acc) ->
    case Bits of
        <<>> ->
            Acc;
        <<Byte, Rest/bitstring>> ->
            inspect@bit_array_pieces(Rest, [erlang:integer_to_binary(Byte) | Acc]);
        _ ->
            Size = erlang:bit_size(Bits),
            <<RemainingBits:Size>> = Bits,
            SizeString = [":size(", erlang:integer_to_binary(Size), ")"],
            Piece = [erlang:integer_to_binary(RemainingBits), SizeString],
            [Piece | Acc]
    end.

inspect@binary(Binary) ->
    case inspect@maybe_utf8_string(Binary, <<>>) of
        {ok, InspectedUtf8String} ->
            InspectedUtf8String;
        {error, not_a_utf8_string} ->
            Segments = [erlang:integer_to_list(X) || <<X>> <= Binary],
            ["<<", lists:join(", ", Segments), ">>"]
    end.

inspect@atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    case inspect@maybe_gleam_atom(Binary, none, <<>>) of
        {ok, Inspected} -> Inspected;
        {error, _} -> ["atom.create_from_string(\"", Binary, "\")"]
    end.

inspect@list(List) ->
    case inspect@proper_or_improper_list(List) of
        {proper, Elements} -> ["[", Elements, "]"];
        {improper, Elements} -> ["//erl([", Elements, "])"]
    end.

inspect@map(Map) ->
    Fields = [
        [<<"#(">>, echo@inspect(Key), <<", ">>, echo@inspect(Value), <<")">>]
        || {Key, Value} <- maps:to_list(Map)
    ],
    ["dict.from_list([", lists:join(", ", Fields), "])"].

inspect@record(Record) ->
    [Atom | ArgsList] = Tuple = erlang:tuple_to_list(Record),
    case inspect@maybe_gleam_atom(Atom, none, <<>>) of
        {ok, Tag} ->
            Args = lists:join(", ", lists:map(fun echo@inspect/1, ArgsList)),
            [Tag, "(", Args, ")"];
        _ ->
            inspect@tuple(Tuple)
    end.

inspect@tuple(Tuple) when erlang:is_tuple(Tuple) ->
    inspect@tuple(erlang:tuple_to_list(Tuple));
inspect@tuple(Tuple) ->
    Elements = lists:map(fun echo@inspect/1, Tuple),
    ["#(", lists:join(", ", Elements), ")"].

inspect@function(Function) ->
    {arity, Arity} = erlang:fun_info(Function, arity),
    ArgsAsciiCodes = lists:seq($a, $a + Arity - 1),
    Args = lists:join(", ", lists:map(fun(Arg) -> <<Arg>> end, ArgsAsciiCodes)),
    ["//fn(", Args, ") { ... }"].

inspect@maybe_utf8_string(Binary, Acc) ->
    case Binary of
        <<>> ->
            {ok, <<$", Acc/binary, $">>};
        <<First/utf8, Rest/binary>> ->
            Escaped = inspect@escape_grapheme(First),
            inspect@maybe_utf8_string(Rest, <<Acc/binary, Escaped/binary>>);
        _ ->
            {error, not_a_utf8_string}
    end.

inspect@escape_grapheme(Char) ->
    case Char of
        $" -> <<$\\, $">>;
        $\\ -> <<$\\, $\\>>;
        $\r -> <<$\\, $r>>;
        $\n -> <<$\\, $n>>;
        $\t -> <<$\\, $t>>;
        $\f -> <<$\\, $f>>;
        X when X > 126, X < 160 -> inspect@convert_to_u(X);
        X when X < 32 -> inspect@convert_to_u(X);
        Other -> <<Other/utf8>>
    end.

inspect@convert_to_u(Code) ->
    erlang:list_to_binary(io_lib:format("\\u{~4.16.0B}", [Code])).

inspect@proper_or_improper_list(List) ->
    case List of
        [] ->
            {proper, []};
        [First] ->
            {proper, [echo@inspect(First)]};
        [First | Rest] when erlang:is_list(Rest) ->
            {Kind, Inspected} = inspect@proper_or_improper_list(Rest),
            {Kind, [echo@inspect(First), ", " | Inspected]};
        [First | ImproperRest] ->
            {improper, [echo@inspect(First), " | ", echo@inspect(ImproperRest)]}
    end.

inspect@maybe_gleam_atom(Atom, PrevChar, Acc) when erlang:is_atom(Atom) ->
    Binary = erlang:atom_to_binary(Atom),
    inspect@maybe_gleam_atom(Binary, PrevChar, Acc);
inspect@maybe_gleam_atom(Atom, PrevChar, Acc) ->
    case {Atom, PrevChar} of
        {<<>>, none} ->
            {error, nil};
        {<<First, _/binary>>, none} when ?is_digit_char(First) ->
            {error, nil};
        {<<"_", _/binary>>, none} ->
            {error, nil};
        {<<"_">>, _} ->
            {error, nil};
        {<<"_", _/binary>>, $_} ->
            {error, nil};
        {<<First, _/binary>>, _} when not ?is_atom_char(First) ->
            {error, nil};
        {<<First, Rest/binary>>, none} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<"_", Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, $_, Acc);
        {<<First, Rest/binary>>, $_} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, (inspect@uppercase(First))>>);
        {<<First, Rest/binary>>, _} ->
            inspect@maybe_gleam_atom(Rest, First, <<Acc/binary, First>>);
        {<<>>, _} ->
            {ok, Acc};
        _ ->
            erlang:throw({gleam_error, echo, Atom, PrevChar, Acc})
    end.

inspect@uppercase(X) -> X - 32.

