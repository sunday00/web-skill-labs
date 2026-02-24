-module(exercism@test_runner).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/exercism/test_runner.gleam").
-export([debug/1, main/0]).
-export_type([beam_module/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type beam_module() :: any().

-file("src/exercism/test_runner.gleam", 39).
-spec debug(KFY) -> KFY.
debug(Value) ->
    exercism_test_runner@internal:append_output(gleam@string:inspect(Value)),
    Value.

-file("src/exercism/test_runner.gleam", 48).
-spec run_test(exercism_test_runner@internal:test()) -> exercism_test_runner@internal:test_result().
run_test(The_test) ->
    Result = exercism_test_runner@internal:run_test(The_test),
    case erlang:element(3, Result) of
        none ->
            gleam_stdlib:print(gleam_community@ansi:green(<<"."/utf8>>));

        {some, Error} ->
            gleam_stdlib:println(gleam_community@ansi:red(<<"F"/utf8>>)),
            gleam_stdlib:println(
                exercism_test_runner@internal:print_error(
                    Error,
                    erlang:element(3, The_test),
                    erlang:element(2, The_test)
                )
            )
    end,
    Result.

-file("src/exercism/test_runner.gleam", 44).
-spec run_suite(exercism_test_runner@internal:suite()) -> list(exercism_test_runner@internal:test_result()).
run_suite(Suite) ->
    gleam@list:map(erlang:element(4, Suite), fun run_test/1).

-file("src/exercism/test_runner.gleam", 83).
?DOC(
    " This function is unsafe. It does not verify that the atom is a BEAM module\n"
    " currently loaded by the VM. Don't mess up!\n"
).
-spec get_beam_module(binary()) -> beam_module().
get_beam_module(Name) ->
    Atom@1 = case begin
        _pipe = Name,
        _pipe@1 = gleam@string:replace(_pipe, <<"/"/utf8>>, <<"@"/utf8>>),
        gleam_erlang_ffi:atom_from_string(_pipe@1)
    end of
        {ok, Atom} -> Atom;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"exercism/test_runner"/utf8>>,
                        function => <<"get_beam_module"/utf8>>,
                        line => 84,
                        value => _assert_fail,
                        start => 2278,
                        'end' => 2364,
                        pattern_start => 2289,
                        pattern_end => 2297})
    end,
    gleam_stdlib:identity(Atom@1).

-file("src/exercism/test_runner.gleam", 91).
-spec get_test(
    binary(),
    binary(),
    beam_module(),
    glance:definition(glance:function_())
) -> {ok, exercism_test_runner@internal:test()} | {error, nil}.
get_test(Src, Module_path, Module, Function) ->
    {function, Name, _, _, _, _, {span, Start, End}} = erlang:element(
        3,
        Function
    ),
    gleam@bool:guard(
        not gleam_stdlib:string_ends_with(Name, <<"_test"/utf8>>),
        {error, nil},
        fun() ->
            Src@1 = exercism_test_runner@internal:extract_function_body(
                Src,
                Start,
                End
            ),
            Function@1 = fun() ->
                exercism_test_runner@internal:run_test_function(
                    fun() ->
                        erlang:apply(Module, erlang:binary_to_atom(Name), [])
                    end
                )
            end,
            {ok, {test, Name, Module_path, Function@1, Src@1}}
        end
    ).

-file("src/exercism/test_runner.gleam", 70).
-spec read_module(gleam@erlang@charlist:charlist()) -> exercism_test_runner@internal:suite().
read_module(Filename) ->
    Filename@1 = unicode:characters_to_binary(Filename),
    Name = gleam@string:drop_end(Filename@1, 6),
    Path = <<"test/"/utf8, Filename@1/binary>>,
    Src@1 = case simplifile:read(Path) of
        {ok, Src} -> Src;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"exercism/test_runner"/utf8>>,
                        function => <<"read_module"/utf8>>,
                        line => 74,
                        value => _assert_fail,
                        start => 1853,
                        'end' => 1895,
                        pattern_start => 1864,
                        pattern_end => 1871})
    end,
    Ast@1 = case glance:module(Src@1) of
        {ok, Ast} -> Ast;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"exercism/test_runner"/utf8>>,
                        function => <<"read_module"/utf8>>,
                        line => 75,
                        value => _assert_fail@1,
                        start => 1898,
                        'end' => 1937,
                        pattern_start => 1909,
                        pattern_end => 1916})
    end,
    Module = get_beam_module(Name),
    Tests = gleam@list:filter_map(
        erlang:element(6, Ast@1),
        fun(_capture) -> get_test(Src@1, Path, Module, _capture) end
    ),
    {suite, Name, Path, Tests}.

-file("src/exercism/test_runner.gleam", 18).
-spec main() -> any().
main() ->
    Files@1 = case file:list_dir(<<"test"/utf8>>) of
        {ok, Files} -> Files;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"exercism/test_runner"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 19,
                        value => _assert_fail,
                        start => 443,
                        'end' => 488,
                        pattern_start => 454,
                        pattern_end => 463})
    end,
    Suites = gleam@list:map(Files@1, fun read_module/1),
    Results = gleam@list:flat_map(Suites, fun run_suite/1),
    {Passed, Message} = exercism_test_runner@internal:print_summary(Results),
    gleam_stdlib:println(<<"\n"/utf8, Message/binary>>),
    case case erlang:element(4, argv:load()) of
        [<<"--json-output-path="/utf8, Path/binary>>] ->
            Json = exercism_test_runner@internal:results_to_json(Results),
            simplifile:write(Path, Json);

        [<<"--json-output-path"/utf8>>, Path] ->
            Json = exercism_test_runner@internal:results_to_json(Results),
            simplifile:write(Path, Json);

        _ ->
            {ok, nil}
    end of
        {ok, _} -> nil;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"exercism/test_runner"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 25,
                        value => _assert_fail@1,
                        start => 674,
                        'end' => 906,
                        pattern_start => 685,
                        pattern_end => 690})
    end,
    erlang:halt(case Passed of
            true ->
                0;

            false ->
                1
        end).
