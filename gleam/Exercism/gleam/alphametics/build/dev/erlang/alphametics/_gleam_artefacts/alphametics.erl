-module(alphametics).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/alphametics.gleam").
-export([solve/1, main/0]).
-export_type([puzzle/0]).

-type puzzle() :: {puzzle,
        binary(),
        list(binary()),
        binary(),
        gleam@set:set(integer()),
        list(binary()),
        gleam@set:set(binary()),
        list(binary()),
        integer(),
        integer()}.

-file("src/alphametics.gleam", 201).
-spec extract_tail(puzzle(), integer()) -> {list(binary()), binary()}.
extract_tail(Puzzle, Col) ->
    Left_tails = begin
        _pipe = erlang:element(3, Puzzle),
        gleam@list:map(
            _pipe,
            fun(El) ->
                Col@1 = case Col > string:length(El) of
                    true ->
                        string:length(El);

                    false ->
                        Col
                end,
                _pipe@1 = El,
                gleam@string:slice(_pipe@1, Col@1 * -1, Col@1)
            end
        )
    end,
    {Left_tails,
        begin
            _pipe@2 = erlang:element(4, Puzzle),
            gleam@string:slice(_pipe@2, Col * -1, Col)
        end}.

-file("src/alphametics.gleam", 215).
-spec map_idx_unq(
    {list(binary()), binary()},
    gleam@dict:dict(binary(), {integer(), list(integer())})
) -> {list({integer(), binary()}), list({binary(), integer()})}.
map_idx_unq(Parts, Fixed_dict) ->
    {Ll, R} = Parts,
    Unique_all_chars = begin
        _pipe = Ll,
        _pipe@2 = gleam@list:map(_pipe, fun(El) -> _pipe@1 = El,
                gleam@string:to_graphemes(_pipe@1) end),
        _pipe@3 = gleam@list:flatten(_pipe@2),
        _pipe@5 = lists:append(
            _pipe@3,
            begin
                _pipe@4 = R,
                gleam@string:to_graphemes(_pipe@4)
            end
        ),
        gleam@list:unique(_pipe@5)
    end,
    Unique_all_chars@1 = begin
        _pipe@6 = Unique_all_chars,
        _pipe@7 = gleam@set:from_list(_pipe@6),
        _pipe@10 = gleam@set:difference(
            _pipe@7,
            begin
                _pipe@8 = Fixed_dict,
                _pipe@9 = maps:keys(_pipe@8),
                gleam@set:from_list(_pipe@9)
            end
        ),
        _pipe@11 = gleam@set:to_list(_pipe@10),
        gleam@list:sort(_pipe@11, fun gleam@string:compare/2)
    end,
    {begin
            _pipe@12 = Unique_all_chars@1,
            gleam@list:index_map(_pipe@12, fun(Ch, I) -> {I, Ch} end)
        end,
        begin
            _pipe@13 = Unique_all_chars@1,
            gleam@list:index_map(_pipe@13, fun(Ch@1, I@1) -> {Ch@1, I@1} end)
        end}.

-file("src/alphametics.gleam", 244).
-spec get_avail_remains_ints(
    puzzle(),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    binary(),
    list(binary())
) -> gleam@set:set(integer()).
get_avail_remains_ints(Puzzle, State, Fixed_dict, Cur_ch, After) ->
    Used@1 = case begin
        _pipe = State,
        gleam_stdlib:map_get(_pipe, Cur_ch)
    end of
        {ok, {_, Used}} ->
            Used;

        _ ->
            []
    end,
    Allocated_others = begin
        _pipe@1 = State,
        _pipe@3 = gleam@dict:filter(
            _pipe@1,
            fun(K, _) ->
                not begin
                    _pipe@2 = After,
                    gleam@list:contains(_pipe@2, K)
                end
            end
        ),
        _pipe@4 = maps:values(_pipe@3),
        _pipe@5 = gleam@list:map(
            _pipe@4,
            fun(El) ->
                {Ch, _} = El,
                Ch
            end
        ),
        _pipe@6 = lists:append(_pipe@5, Used@1),
        lists:append(
            _pipe@6,
            begin
                _pipe@7 = Fixed_dict,
                _pipe@8 = maps:values(_pipe@7),
                gleam@list:map(_pipe@8, fun(V) -> erlang:element(1, V) end)
            end
        )
    end,
    Allocated_others@1 = case begin
        _pipe@9 = erlang:element(8, Puzzle),
        gleam@list:contains(_pipe@9, Cur_ch)
    end of
        true ->
            _pipe@10 = Allocated_others,
            gleam@list:prepend(_pipe@10, 0);

        false ->
            Allocated_others
    end,
    _pipe@11 = erlang:element(5, Puzzle),
    gleam@set:difference(
        _pipe@11,
        begin
            _pipe@12 = Allocated_others@1,
            gleam@set:from_list(_pipe@12)
        end
    ).

-file("src/alphametics.gleam", 275).
-spec map_i_to_parts(
    {list(binary()), binary()},
    gleam@dict:dict(binary(), {integer(), list(integer())})
) -> {list(binary()), binary()}.
map_i_to_parts(Parts, State) ->
    Str = <<<<(begin
                _pipe = erlang:element(1, Parts),
                gleam@string:join(_pipe, <<" + "/utf8>>)
            end)/binary,
            " == "/utf8>>/binary,
        (erlang:element(2, Parts))/binary>>,
    {L@1, R@1} = case begin
        _pipe@1 = State,
        _pipe@4 = gleam@dict:fold(
            _pipe@1,
            Str,
            fun(Acc, Ch, S) ->
                {I, _} = S,
                _pipe@2 = Acc,
                gleam@string:replace(
                    _pipe@2,
                    Ch,
                    begin
                        _pipe@3 = I,
                        erlang:integer_to_binary(_pipe@3)
                    end
                )
            end
        ),
        gleam@string:split(_pipe@4, <<" == "/utf8>>)
    end of
        [L, R] -> {L, R};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"alphametics"/utf8>>,
                        function => <<"map_i_to_parts"/utf8>>,
                        line => 281,
                        value => _assert_fail,
                        start => 6515,
                        'end' => 6692,
                        pattern_start => 6526,
                        pattern_end => 6532})
    end,
    {begin
            _pipe@5 = L@1,
            gleam@string:split(_pipe@5, <<" + "/utf8>>)
        end,
        R@1}.

-file("src/alphametics.gleam", 292).
-spec is_correct_parts({list(binary()), binary()}, integer()) -> boolean().
is_correct_parts(Parts, Col) ->
    Divider = begin
        _pipe@1 = gleam@int:power(
            10,
            begin
                _pipe = Col,
                erlang:float(_pipe)
            end
        ),
        _pipe@2 = gleam@result:unwrap(_pipe@1, +0.0),
        erlang:round(_pipe@2)
    end,
    {L, R} = Parts,
    (case Divider of
        0 -> 0;
        Gleam@denominator -> begin
            _pipe@3 = L,
            gleam@list:fold(
                _pipe@3,
                0,
                fun(Acc, Cur) ->
                    Acc + begin
                        _pipe@4 = Cur,
                        _pipe@5 = gleam_stdlib:parse_int(_pipe@4),
                        gleam@result:unwrap(_pipe@5, 0)
                    end
                end
            )
        end
        rem Gleam@denominator
    end) =:= begin
        _pipe@6 = R,
        _pipe@7 = gleam_stdlib:parse_int(_pipe@6),
        gleam@result:unwrap(_pipe@7, 0)
    end.

-file("src/alphametics.gleam", 309).
-spec allocate_next_num(
    integer(),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    {list({integer(), binary()}), list({binary(), integer()})},
    puzzle()
) -> {ok, gleam@dict:dict(binary(), {integer(), list(integer())})} |
    {error, nil}.
allocate_next_num(Id, State, Fixed_dict, Map, Puzzle) ->
    {I_ch, _} = Map,
    Letters_len = erlang:length(I_ch),
    case Id of
        Id@1 when Id@1 >= 0 ->
            Cur_ch@1 = case begin
                _pipe = I_ch,
                gleam@list:key_find(_pipe, Id@1)
            end of
                {ok, Cur_ch} -> Cur_ch;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"alphametics"/utf8>>,
                                function => <<"allocate_next_num"/utf8>>,
                                line => 321,
                                value => _assert_fail,
                                start => 7400,
                                'end' => 7449,
                                pattern_start => 7411,
                                pattern_end => 7421})
            end,
            Remains = get_avail_remains_ints(
                Puzzle,
                State,
                Fixed_dict,
                Cur_ch@1,
                begin
                    _pipe@1 = I_ch,
                    gleam@list:filter_map(
                        _pipe@1,
                        fun(El) -> case erlang:element(1, El) > Id@1 of
                                true ->
                                    {ok, erlang:element(2, El)};

                                false ->
                                    {error, nil}
                            end end
                    )
                end
            ),
            case begin
                _pipe@2 = Remains,
                gleam@set:size(_pipe@2)
            end
            > 0 of
                true ->
                    N = begin
                        _pipe@3 = Remains,
                        _pipe@4 = gleam@set:to_list(_pipe@3),
                        _pipe@5 = gleam@list:first(_pipe@4),
                        gleam@result:unwrap(_pipe@5, -1)
                    end,
                    New_state = begin
                        _pipe@6 = State,
                        gleam@dict:upsert(_pipe@6, Cur_ch@1, fun(X) -> case X of
                                    {some, Xx} ->
                                        {Prev, Used} = Xx,
                                        {N,
                                            begin
                                                _pipe@7 = Used,
                                                gleam@list:prepend(
                                                    _pipe@7,
                                                    Prev
                                                )
                                            end};

                                    none ->
                                        {N, []}
                                end end)
                    end,
                    case (erlang:length(I_ch) - 1) > Id@1 of
                        true ->
                            New_state@1 = begin
                                _pipe@8 = I_ch,
                                gleam@list:fold(
                                    _pipe@8,
                                    New_state,
                                    fun(Acc, El@1) ->
                                        {I, Ch} = El@1,
                                        case I > Id@1 of
                                            false ->
                                                Acc;

                                            true ->
                                                _pipe@9 = Acc,
                                                gleam@dict:delete(_pipe@9, Ch)
                                        end
                                    end
                                )
                            end,
                            allocate_next_num(
                                Id@1 + 1,
                                New_state@1,
                                Fixed_dict,
                                Map,
                                Puzzle
                            );

                        false ->
                            {ok, New_state}
                    end;

                false ->
                    case Id@1 =:= 0 of
                        true ->
                            {error, nil};

                        false ->
                            allocate_next_num(
                                Id@1 - 1,
                                State,
                                Fixed_dict,
                                Map,
                                Puzzle
                            )
                    end
            end;

        Id@2 when Id@2 > Letters_len ->
            {ok, State};

        _ ->
            {error, nil}
    end.

-file("src/alphametics.gleam", 160).
-spec get_next_state(
    puzzle(),
    {list(binary()), binary()},
    {list({integer(), binary()}), list({binary(), integer()})},
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    integer(),
    integer()
) -> {ok, gleam@dict:dict(binary(), {integer(), list(integer())})} |
    {error, nil}.
get_next_state(Puzzle, Parts, Indexd_map, State, Fixed_dict, Col, Id) ->
    {I_ch, _} = Indexd_map,
    State@1 = allocate_next_num(Id, State, Fixed_dict, Indexd_map, Puzzle),
    case State@1 of
        {ok, S} ->
            Ms = maps:merge(S, Fixed_dict),
            Paragraph = map_i_to_parts(Parts, Ms),
            case is_correct_parts(Paragraph, Col) of
                true ->
                    {ok, Ms};

                false ->
                    get_next_state(
                        Puzzle,
                        Parts,
                        Indexd_map,
                        S,
                        Fixed_dict,
                        Col,
                        erlang:length(I_ch) - 1
                    )
            end;

        {error, _} ->
            {error, nil}
    end.

-file("src/alphametics.gleam", 67).
-spec processing(
    puzzle(),
    gleam@dict:dict(binary(), {integer(), list(integer())}),
    gleam@option:option(gleam@dict:dict(binary(), {integer(), list(integer())})),
    list({gleam@dict:dict(binary(), {integer(), list(integer())}), integer()}),
    integer()
) -> {ok, gleam@dict:dict(binary(), {integer(), list(integer())})} |
    {error, nil}.
processing(Puzzle, Fixed_dict, Snap_shot, Snap_shots, Col) ->
    case erlang:element(9, Puzzle) < Col of
        true ->
            {ok, Fixed_dict};

        false ->
            Parts = extract_tail(Puzzle, Col),
            Indexd_map = map_idx_unq(Parts, Fixed_dict),
            State = case Snap_shot of
                {some, S} ->
                    get_next_state(
                        Puzzle,
                        Parts,
                        Indexd_map,
                        begin
                            _pipe = S,
                            gleam@dict:drop(
                                _pipe,
                                begin
                                    _pipe@1 = Fixed_dict,
                                    maps:keys(_pipe@1)
                                end
                            )
                        end,
                        Fixed_dict,
                        Col,
                        begin
                            _pipe@4 = begin
                                _pipe@2 = S,
                                gleam@dict:drop(
                                    _pipe@2,
                                    begin
                                        _pipe@3 = Fixed_dict,
                                        maps:keys(_pipe@3)
                                    end
                                )
                            end,
                            maps:size(_pipe@4)
                        end
                        - 1
                    );

                none ->
                    case begin
                        _pipe@5 = erlang:element(1, Indexd_map),
                        erlang:length(_pipe@5)
                    end
                    =:= 0 of
                        true ->
                            Paragraph = map_i_to_parts(Parts, Fixed_dict),
                            case is_correct_parts(Paragraph, Col) of
                                true ->
                                    {ok, Fixed_dict};

                                false ->
                                    {error, nil}
                            end;

                        false ->
                            get_next_state(
                                Puzzle,
                                Parts,
                                Indexd_map,
                                maps:new(),
                                Fixed_dict,
                                Col,
                                0
                            )
                    end
            end,
            case State of
                {ok, S@1} ->
                    processing(
                        Puzzle,
                        S@1,
                        none,
                        begin
                            _pipe@6 = Snap_shots,
                            gleam@list:prepend(_pipe@6, {S@1, Col})
                        end,
                        Col + 1
                    );

                {error, _} ->
                    case Snap_shots of
                        [Ls] ->
                            {Fixed_dict@1, Col@1} = Ls,
                            processing(
                                Puzzle,
                                maps:new(),
                                {some, Fixed_dict@1},
                                [],
                                Col@1
                            );

                        [Ls@1, Ls2 | R] ->
                            {Fixed_dict@2, Col@2} = Ls@1,
                            {Fixed_dict2, _} = Ls2,
                            processing(
                                Puzzle,
                                Fixed_dict2,
                                {some, Fixed_dict@2},
                                [Ls2 | R],
                                Col@2
                            );

                        _ ->
                            {error, nil}
                    end
            end
    end.

-file("src/alphametics.gleam", 58).
-spec resolve(puzzle()) -> {ok, gleam@dict:dict(binary(), integer())} |
    {error, nil}.
resolve(Puzzle) ->
    case processing(Puzzle, maps:new(), none, [], 1) of
        {ok, P} ->
            {ok,
                begin
                    _pipe = P,
                    gleam@dict:map_values(
                        _pipe,
                        fun(_, V) -> erlang:element(1, V) end
                    )
                end};

        _ ->
            {error, nil}
    end.

-file("src/alphametics.gleam", 25).
-spec solve(binary()) -> {ok, gleam@dict:dict(binary(), integer())} |
    {error, nil}.
solve(Puzzle) ->
    {Left@1, Right@1} = case begin
        _pipe = Puzzle,
        gleam@string:split(_pipe, <<" == "/utf8>>)
    end of
        [Left, Right] -> {Left, Right};
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"alphametics"/utf8>>,
                        function => <<"solve"/utf8>>,
                        line => 26,
                        value => _assert_fail,
                        start => 514,
                        'end' => 571,
                        pattern_start => 525,
                        pattern_end => 538})
    end,
    All_i = begin
        _pipe@1 = gleam@list:range(0, 9),
        gleam@set:from_list(_pipe@1)
    end,
    All_words = begin
        _pipe@2 = Left@1,
        _pipe@3 = gleam@string:split(_pipe@2, <<" + "/utf8>>),
        gleam@list:prepend(_pipe@3, Right@1)
    end,
    All_chars = begin
        _pipe@4 = All_words,
        _pipe@7 = gleam@list:fold(_pipe@4, [], fun(Acc, Cur) -> _pipe@5 = Acc,
                lists:append(
                    _pipe@5,
                    begin
                        _pipe@6 = Cur,
                        gleam@string:to_graphemes(_pipe@6)
                    end
                ) end),
        gleam@set:from_list(_pipe@7)
    end,
    No_zeros = begin
        _pipe@8 = All_words,
        gleam@list:map(_pipe@8, fun(El) -> _pipe@9 = El,
                _pipe@10 = gleam@string:first(_pipe@9),
                gleam@result:unwrap(_pipe@10, <<""/utf8>>) end)
    end,
    Puzzle@1 = {puzzle,
        Puzzle,
        begin
            _pipe@11 = Left@1,
            gleam@string:split(_pipe@11, <<" + "/utf8>>)
        end,
        Right@1,
        All_i,
        All_words,
        All_chars,
        No_zeros,
        begin
            _pipe@12 = All_words,
            gleam@list:fold(_pipe@12, 0, fun(Acc@1, Cur@1) -> _pipe@13 = Acc@1,
                    gleam@int:max(
                        _pipe@13,
                        begin
                            _pipe@14 = Cur@1,
                            string:length(_pipe@14)
                        end
                    ) end)
        end,
        begin
            _pipe@15 = Left@1,
            _pipe@16 = gleam@string:split(_pipe@15, <<" + "/utf8>>),
            gleam@list:fold(
                _pipe@16,
                0,
                fun(Acc@2, El@1) ->
                    gleam@int:max(
                        Acc@2,
                        begin
                            _pipe@17 = El@1,
                            string:length(_pipe@17)
                        end
                    )
                end
            )
        end},
    resolve(Puzzle@1).

-file("src/alphametics.gleam", 388).
-spec main() -> {ok, gleam@dict:dict(binary(), integer())} | {error, nil}.
main() ->
    echo(solve(<<"HE + SEES + THE == LIGHT"/utf8>>), 389).

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

