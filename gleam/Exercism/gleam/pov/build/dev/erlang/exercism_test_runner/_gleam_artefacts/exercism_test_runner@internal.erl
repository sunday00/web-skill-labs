-module(exercism_test_runner@internal).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/exercism_test_runner/internal.gleam").
-export([extract_function_body/3, print_error/3, print_summary/1, run_test_function/1, append_output/1, clear_output/0, get_output/0, run_test/1, results_to_json/1]).
-export_type([error/0, suite/0, test/0, test_result/0, process_dictionary_key/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type error() :: {unequal, gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()} |
    {todo, binary(), binary(), integer()} |
    {panic, binary(), binary(), integer()} |
    {unmatched, gleam@dynamic:dynamic_(), binary(), integer()} |
    {unmatched_case, gleam@dynamic:dynamic_()} |
    {crashed, gleam@dynamic:dynamic_()}.

-type suite() :: {suite, binary(), binary(), list(test())}.

-type test() :: {test,
        binary(),
        binary(),
        fun(() -> {ok, nil} | {error, error()}),
        binary()}.

-type test_result() :: {test_result,
        test(),
        gleam@option:option(error()),
        binary()}.

-type process_dictionary_key() :: execism_test_runner_user_output.

-file("src/exercism_test_runner/internal.gleam", 56).
?DOC(false).
-spec drop_function_header(binary()) -> binary().
drop_function_header(Src) ->
    case gleam_stdlib:string_pop_grapheme(Src) of
        {ok, {<<"{"/utf8>>, Src@1}} ->
            Src@1;

        {ok, {_, Src@2}} ->
            drop_function_header(Src@2);

        {error, _} ->
            Src
    end.

-file("src/exercism_test_runner/internal.gleam", 64).
?DOC(false).
-spec undent(binary()) -> binary().
undent(Line) ->
    case gleam_stdlib:string_starts_with(Line, <<"  "/utf8>>) of
        true ->
            gleam@string:drop_start(Line, 2);

        false ->
            Line
    end.

-file("src/exercism_test_runner/internal.gleam", 41).
?DOC(false).
-spec extract_function_body(binary(), integer(), integer()) -> binary().
extract_function_body(Src, Start, End) ->
    _pipe = Src,
    _pipe@1 = gleam_stdlib:identity(_pipe),
    _pipe@2 = gleam_stdlib:bit_array_slice(_pipe@1, Start, End - Start),
    _pipe@3 = gleam@result:unwrap(_pipe@2, <<>>),
    _pipe@4 = gleam@bit_array:to_string(_pipe@3),
    _pipe@5 = gleam@result:unwrap(_pipe@4, <<""/utf8>>),
    _pipe@6 = gleam@string:drop_end(_pipe@5, 1),
    _pipe@7 = drop_function_header(_pipe@6),
    _pipe@8 = gleam@string:trim(_pipe@7),
    _pipe@9 = gleam@string:split(_pipe@8, <<"\n"/utf8>>),
    _pipe@10 = gleam@list:map(_pipe@9, fun undent/1),
    gleam@string:join(_pipe@10, <<"\n"/utf8>>).

-file("src/exercism_test_runner/internal.gleam", 71).
?DOC(false).
-spec print_properties(binary(), list({binary(), binary()})) -> binary().
print_properties(Header, Properties) ->
    _pipe = Properties,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(Pair) ->
            Key = <<(gleam@string:pad_start(
                    erlang:element(1, Pair),
                    7,
                    <<" "/utf8>>
                ))/binary,
                ": "/utf8>>,
            <<(gleam_community@ansi:cyan(Key))/binary,
                (erlang:element(2, Pair))/binary>>
        end
    ),
    _pipe@2 = gleam@list:prepend(_pipe@1, Header),
    gleam@string:join(_pipe@2, <<"\n"/utf8>>).

-file("src/exercism_test_runner/internal.gleam", 84).
?DOC(false).
-spec print_error(error(), binary(), binary()) -> binary().
print_error(Error, Path, Test_name) ->
    case Error of
        {unequal, Left, Right} ->
            {Left_colored, Right_colored} = case {gleam@dynamic@decode:run(
                    Left,
                    {decoder, fun gleam@dynamic@decode:decode_bool/1}
                ),
                gleam@dynamic@decode:run(
                    Right,
                    {decoder, fun gleam@dynamic@decode:decode_bool/1}
                )} of
                {{ok, Left_bool}, {ok, Right_bool}} ->
                    {begin
                            _pipe = Left_bool,
                            _pipe@1 = gleam@string:inspect(_pipe),
                            _pipe@2 = gleam_community@ansi:green(_pipe@1),
                            gleam_community@ansi:bold(_pipe@2)
                        end,
                        begin
                            _pipe@3 = Right_bool,
                            _pipe@4 = gleam@string:inspect(_pipe@3),
                            _pipe@5 = gleam_community@ansi:red(_pipe@4),
                            gleam_community@ansi:bold(_pipe@5)
                        end};

                {_, _} ->
                    Diff = gap:to_styled(
                        gap:compare_strings(
                            gleam@string:inspect(Left),
                            gleam@string:inspect(Right)
                        )
                    ),
                    {erlang:element(2, Diff), erlang:element(3, Diff)}
            end,
            _pipe@6 = Path,
            print_properties(
                _pipe@6,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"left != right"/utf8>>},
                    {<<"left"/utf8>>, Left_colored},
                    {<<"right"/utf8>>, Right_colored}]
            );

        {todo, Message, Module, Line} ->
            print_properties(
                Path,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"todo"/utf8>>},
                    {<<"site"/utf8>>,
                        <<<<Module/binary, ":"/utf8>>/binary,
                            (erlang:integer_to_binary(Line))/binary>>},
                    {<<"info"/utf8>>, Message}]
            );

        {panic, Message@1, Module@1, Line@1} ->
            print_properties(
                Path,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"panic"/utf8>>},
                    {<<"site"/utf8>>,
                        <<<<Module@1/binary, ":"/utf8>>/binary,
                            (erlang:integer_to_binary(Line@1))/binary>>},
                    {<<"info"/utf8>>, Message@1}]
            );

        {unmatched, Value, Module@2, Line@2} ->
            print_properties(
                Path,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"Pattern match failed"/utf8>>},
                    {<<"site"/utf8>>,
                        <<<<Module@2/binary, ":"/utf8>>/binary,
                            (erlang:integer_to_binary(Line@2))/binary>>},
                    {<<"value"/utf8>>, gleam@string:inspect(Value)}]
            );

        {unmatched_case, Value@1} ->
            print_properties(
                Path,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"Pattern match failed"/utf8>>},
                    {<<"value"/utf8>>, gleam@string:inspect(Value@1)}]
            );

        {crashed, Error@1} ->
            print_properties(
                Path,
                [{<<"test"/utf8>>, Test_name},
                    {<<"error"/utf8>>, <<"Program crashed"/utf8>>},
                    {<<"cause"/utf8>>, gleam@string:inspect(Error@1)}]
            )
    end.

-file("src/exercism_test_runner/internal.gleam", 160).
?DOC(false).
-spec print_summary(list(test_result())) -> {boolean(), binary()}.
print_summary(Results) ->
    Total = begin
        _pipe = Results,
        _pipe@1 = erlang:length(_pipe),
        erlang:integer_to_binary(_pipe@1)
    end,
    Failed = begin
        _pipe@2 = Results,
        _pipe@3 = gleam@list:filter(
            _pipe@2,
            fun(Result) -> erlang:element(3, Result) /= none end
        ),
        _pipe@4 = erlang:length(_pipe@3),
        erlang:integer_to_binary(_pipe@4)
    end,
    Colour = case Failed of
        <<"0"/utf8>> ->
            fun gleam_community@ansi:green/1;

        _ ->
            fun gleam_community@ansi:red/1
    end,
    Message = Colour(
        <<<<<<<<"Ran "/utf8, Total/binary>>/binary, " tests, "/utf8>>/binary,
                Failed/binary>>/binary,
            " failed"/utf8>>
    ),
    {Failed =:= <<"0"/utf8>>, Message}.

-file("src/exercism_test_runner/internal.gleam", 253).
?DOC(false).
-spec decode_tag(any()) -> gleam@dynamic@decode:decoder(nil).
decode_tag(Tag) ->
    gleam@dynamic@decode:new_primitive_decoder(
        <<"Tag"/utf8>>,
        fun(Data) -> case gleam_stdlib:identity(Tag) =:= Data of
                true ->
                    {ok, nil};

                false ->
                    {error, nil}
            end end
    ).

-file("src/exercism_test_runner/internal.gleam", 206).
?DOC(false).
-spec decode_pattern_match_failed_error() -> gleam@dynamic@decode:decoder(error()).
decode_pattern_match_failed_error() ->
    gleam@dynamic@decode:field(
        erlang:binary_to_atom(<<"gleam_error"/utf8>>),
        decode_tag(erlang:binary_to_atom(<<"let_assert"/utf8>>)),
        fun(_) ->
            gleam@dynamic@decode:field(
                erlang:binary_to_atom(<<"value"/utf8>>),
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                fun(Value) ->
                    gleam@dynamic@decode:field(
                        erlang:binary_to_atom(<<"module"/utf8>>),
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        fun(Module) ->
                            gleam@dynamic@decode:field(
                                erlang:binary_to_atom(<<"line"/utf8>>),
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(Line) ->
                                    gleam@dynamic@decode:success(
                                        {unmatched, Value, Module, Line}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/exercism_test_runner/internal.gleam", 217).
?DOC(false).
-spec decode_todo_error() -> gleam@dynamic@decode:decoder(error()).
decode_todo_error() ->
    gleam@dynamic@decode:field(
        erlang:binary_to_atom(<<"gleam_error"/utf8>>),
        decode_tag(erlang:binary_to_atom(<<"todo"/utf8>>)),
        fun(_) ->
            gleam@dynamic@decode:field(
                erlang:binary_to_atom(<<"message"/utf8>>),
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Message) ->
                    gleam@dynamic@decode:field(
                        erlang:binary_to_atom(<<"module"/utf8>>),
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        fun(Module) ->
                            gleam@dynamic@decode:field(
                                erlang:binary_to_atom(<<"line"/utf8>>),
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(Line) ->
                                    gleam@dynamic@decode:success(
                                        {todo, Message, Module, Line}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/exercism_test_runner/internal.gleam", 228).
?DOC(false).
-spec decode_panic_error() -> gleam@dynamic@decode:decoder(error()).
decode_panic_error() ->
    gleam@dynamic@decode:field(
        erlang:binary_to_atom(<<"gleam_error"/utf8>>),
        decode_tag(erlang:binary_to_atom(<<"panic"/utf8>>)),
        fun(_) ->
            gleam@dynamic@decode:field(
                erlang:binary_to_atom(<<"message"/utf8>>),
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Message) ->
                    gleam@dynamic@decode:field(
                        erlang:binary_to_atom(<<"module"/utf8>>),
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        fun(Module) ->
                            gleam@dynamic@decode:field(
                                erlang:binary_to_atom(<<"line"/utf8>>),
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(Line) ->
                                    gleam@dynamic@decode:success(
                                        {panic, Message, Module, Line}
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/exercism_test_runner/internal.gleam", 239).
?DOC(false).
-spec decode_case_clause_error() -> gleam@dynamic@decode:decoder(error()).
decode_case_clause_error() ->
    gleam@dynamic@decode:field(
        0,
        decode_tag(erlang:binary_to_atom(<<"case_clause"/utf8>>)),
        fun(_) ->
            gleam@dynamic@decode:field(
                1,
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                fun(Value) ->
                    gleam@dynamic@decode:success({unmatched_case, Value})
                end
            )
        end
    ).

-file("src/exercism_test_runner/internal.gleam", 245).
?DOC(false).
-spec decode_unequal_error() -> gleam@dynamic@decode:decoder(error()).
decode_unequal_error() ->
    Tag = erlang:binary_to_atom(<<"unequal"/utf8>>),
    gleam@dynamic@decode:field(
        0,
        decode_tag(Tag),
        fun(_) ->
            gleam@dynamic@decode:field(
                1,
                {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                fun(A) ->
                    gleam@dynamic@decode:field(
                        2,
                        {decoder, fun gleam@dynamic@decode:decode_dynamic/1},
                        fun(B) ->
                            gleam@dynamic@decode:success({unequal, A, B})
                        end
                    )
                end
            )
        end
    ).

-file("src/exercism_test_runner/internal.gleam", 186).
?DOC(false).
-spec convert_error(gleam@erlang:crash()) -> error().
convert_error(Error) ->
    case Error of
        {exited, Error@1} ->
            {crashed, Error@1};

        {thrown, Error@2} ->
            {crashed, Error@2};

        {errored, Error@3} ->
            Decoders = gleam@dynamic@decode:one_of(
                decode_unequal_error(),
                [decode_pattern_match_failed_error(),
                    decode_case_clause_error(),
                    decode_todo_error(),
                    decode_panic_error()]
            ),
            _pipe = Error@3,
            _pipe@1 = gleam@dynamic@decode:run(_pipe, Decoders),
            gleam@result:unwrap(_pipe@1, {crashed, Error@3})
    end.

-file("src/exercism_test_runner/internal.gleam", 179).
?DOC(false).
-spec run_test_function(fun(() -> any())) -> {ok, nil} | {error, error()}.
run_test_function(Function) ->
    _pipe = Function,
    _pipe@1 = gleam_erlang_ffi:rescue(_pipe),
    _pipe@2 = gleam@result:map_error(_pipe@1, fun convert_error/1),
    gleam@result:replace(_pipe@2, nil).

-file("src/exercism_test_runner/internal.gleam", 276).
?DOC(false).
-spec append_output(binary()) -> nil.
append_output(Value) ->
    _pipe = execism_test_runner_user_output,
    _pipe@1 = erlang:get(_pipe),
    _pipe@2 = gleam@dynamic@decode:run(
        _pipe@1,
        {decoder, fun gleam@dynamic@decode:decode_string/1}
    ),
    _pipe@3 = gleam@result:unwrap(_pipe@2, <<""/utf8>>),
    _pipe@4 = gleam@string:append(_pipe@3, Value),
    _pipe@5 = gleam@string:append(_pipe@4, <<"\n"/utf8>>),
    erlang:put(execism_test_runner_user_output, _pipe@5),
    nil.

-file("src/exercism_test_runner/internal.gleam", 287).
?DOC(false).
-spec clear_output() -> nil.
clear_output() ->
    erlang:put(execism_test_runner_user_output, <<""/utf8>>),
    nil.

-file("src/exercism_test_runner/internal.gleam", 292).
?DOC(false).
-spec get_output() -> binary().
get_output() ->
    _pipe = execism_test_runner_user_output,
    _pipe@1 = erlang:get(_pipe),
    _pipe@2 = gleam@dynamic@decode:run(
        _pipe@1,
        {decoder, fun gleam@dynamic@decode:decode_string/1}
    ),
    gleam@result:unwrap(_pipe@2, <<""/utf8>>).

-file("src/exercism_test_runner/internal.gleam", 299).
?DOC(false).
-spec run_test(test()) -> test_result().
run_test(The_test) ->
    clear_output(),
    Error@1 = case (erlang:element(4, The_test))() of
        {ok, _} ->
            none;

        {error, Error} ->
            {some, Error}
    end,
    {test_result, The_test, Error@1, get_output()}.

-file("src/exercism_test_runner/internal.gleam", 343).
?DOC(false).
-spec truncate(binary()) -> binary().
truncate(Output) ->
    case string:length(Output) > 500 of
        true ->
            _pipe = Output,
            _pipe@1 = gleam@string:slice(_pipe, 0, 448),
            _pipe@2 = gleam@string:append(_pipe@1, <<"...\n\n"/utf8>>),
            gleam@string:append(
                _pipe@2,
                <<"Output was truncated. Please limit to 500 chars"/utf8>>
            );

        false ->
            Output
    end.

-file("src/exercism_test_runner/internal.gleam", 322).
?DOC(false).
-spec test_result_json(test_result()) -> gleam@json:json().
test_result_json(Result) ->
    Fields = case erlang:element(3, Result) of
        {some, Error} ->
            Error@1 = print_error(
                Error,
                erlang:element(3, erlang:element(2, Result)),
                erlang:element(2, erlang:element(2, Result))
            ),
            [{<<"status"/utf8>>, gleam@json:string(<<"fail"/utf8>>)},
                {<<"message"/utf8>>, gleam@json:string(Error@1)}];

        none ->
            [{<<"status"/utf8>>, gleam@json:string(<<"pass"/utf8>>)}]
    end,
    Fields@1 = case truncate(erlang:element(4, Result)) of
        <<""/utf8>> ->
            Fields;

        Output ->
            [{<<"output"/utf8>>, gleam@json:string(Output)} | Fields]
    end,
    Fields@2 = [{<<"name"/utf8>>,
            gleam@json:string(erlang:element(2, erlang:element(2, Result)))},
        {<<"test_code"/utf8>>,
            gleam@json:string(erlang:element(5, erlang:element(2, Result)))} |
        Fields@1],
    gleam@json:object(Fields@2).

-file("src/exercism_test_runner/internal.gleam", 308).
?DOC(false).
-spec results_to_json(list(test_result())) -> binary().
results_to_json(Results) ->
    Failed = gleam@list:any(
        Results,
        fun(The_test) -> erlang:element(3, The_test) /= none end
    ),
    Status = case Failed of
        true ->
            <<"fail"/utf8>>;

        false ->
            <<"pass"/utf8>>
    end,
    _pipe = gleam@json:object(
        [{<<"version"/utf8>>, gleam@json:int(2)},
            {<<"status"/utf8>>, gleam@json:string(Status)},
            {<<"tests"/utf8>>,
                gleam@json:array(Results, fun test_result_json/1)}]
    ),
    gleam@json:to_string(_pipe).
