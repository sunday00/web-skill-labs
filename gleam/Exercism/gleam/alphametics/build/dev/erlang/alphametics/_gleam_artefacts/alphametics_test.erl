-module(alphametics_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/alphametics_test.gleam").
-export([main/0, puzzle_with_three_letters_test/0, solution_must_have_unique_value_for_each_letter_test/0, leading_zero_solution_is_invalid_test/0, puzzle_with_two_digits_final_carry_test/0, puzzle_with_four_letters_test/0, puzzle_with_six_letters_test/0, puzzle_with_seven_letters_test/0, puzzle_with_eight_letters_test/0, puzzle_with_ten_letters_test/0]).

-file("test/alphametics_test.gleam", 6).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/alphametics_test.gleam", 10).
-spec puzzle_with_three_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_three_letters_test() ->
    _pipe = alphametics:solve(<<"I + BB == ILL"/utf8>>),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"B"/utf8>>, 9}, {<<"I"/utf8>>, 1}, {<<"L"/utf8>>, 0}]
            )}
    ).

-file("test/alphametics_test.gleam", 15).
-spec solution_must_have_unique_value_for_each_letter_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
solution_must_have_unique_value_for_each_letter_test() ->
    _pipe = alphametics:solve(<<"A == B"/utf8>>),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/alphametics_test.gleam", 20).
-spec leading_zero_solution_is_invalid_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
leading_zero_solution_is_invalid_test() ->
    _pipe = alphametics:solve(<<"ACA + DD == BD"/utf8>>),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/alphametics_test.gleam", 25).
-spec puzzle_with_two_digits_final_carry_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_two_digits_final_carry_test() ->
    _pipe = alphametics:solve(
        <<"A + A + A + A + A + A + A + A + A + A + A + B == BCC"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"A"/utf8>>, 9}, {<<"B"/utf8>>, 1}, {<<"C"/utf8>>, 0}]
            )}
    ).

-file("test/alphametics_test.gleam", 30).
-spec puzzle_with_four_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_four_letters_test() ->
    _pipe = alphametics:solve(<<"AS + A == MOM"/utf8>>),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"A"/utf8>>, 9},
                    {<<"M"/utf8>>, 1},
                    {<<"O"/utf8>>, 0},
                    {<<"S"/utf8>>, 2}]
            )}
    ).

-file("test/alphametics_test.gleam", 37).
-spec puzzle_with_six_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_six_letters_test() ->
    _pipe = alphametics:solve(<<"NO + NO + TOO == LATE"/utf8>>),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"A"/utf8>>, 0},
                    {<<"E"/utf8>>, 2},
                    {<<"L"/utf8>>, 1},
                    {<<"N"/utf8>>, 7},
                    {<<"O"/utf8>>, 4},
                    {<<"T"/utf8>>, 9}]
            )}
    ).

-file("test/alphametics_test.gleam", 53).
-spec puzzle_with_seven_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_seven_letters_test() ->
    _pipe = alphametics:solve(<<"HE + SEES + THE == LIGHT"/utf8>>),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"E"/utf8>>, 4},
                    {<<"G"/utf8>>, 2},
                    {<<"H"/utf8>>, 5},
                    {<<"I"/utf8>>, 0},
                    {<<"L"/utf8>>, 1},
                    {<<"S"/utf8>>, 9},
                    {<<"T"/utf8>>, 7}]
            )}
    ).

-file("test/alphametics_test.gleam", 70).
-spec puzzle_with_eight_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_eight_letters_test() ->
    _pipe = alphametics:solve(<<"SEND + MORE == MONEY"/utf8>>),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"D"/utf8>>, 7},
                    {<<"E"/utf8>>, 5},
                    {<<"M"/utf8>>, 1},
                    {<<"N"/utf8>>, 6},
                    {<<"O"/utf8>>, 0},
                    {<<"R"/utf8>>, 8},
                    {<<"S"/utf8>>, 9},
                    {<<"Y"/utf8>>, 2}]
            )}
    ).

-file("test/alphametics_test.gleam", 88).
-spec puzzle_with_ten_letters_test() -> {ok,
        gleam@dict:dict(binary(), integer())} |
    {error, nil}.
puzzle_with_ten_letters_test() ->
    _pipe = alphametics:solve(
        <<"AND + A + STRONG + OFFENSE + AS + A + GOOD == DEFENSE"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok,
            maps:from_list(
                [{<<"A"/utf8>>, 5},
                    {<<"D"/utf8>>, 3},
                    {<<"E"/utf8>>, 4},
                    {<<"F"/utf8>>, 7},
                    {<<"G"/utf8>>, 8},
                    {<<"N"/utf8>>, 0},
                    {<<"O"/utf8>>, 2},
                    {<<"R"/utf8>>, 1},
                    {<<"S"/utf8>>, 6},
                    {<<"T"/utf8>>, 9}]
            )}
    ).
