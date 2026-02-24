-module(change_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/change_test.gleam").
-export([main/0, change_for_1_cent_test/0, single_coin_change_test/0, multiple_coin_change_test/0, change_with_lilliputian_coins_test/0, change_with_lower_elbonia_coins_test/0, large_target_values_test/0, possible_change_without_unit_coins_available_test/0, another_possible_change_without_unit_coins_available_test/0, no_coins_make_0_change_test/0, error_testing_for_change_smaller_than_the_smallest_of_coins_test/0, error_if_no_combination_can_add_up_to_target_test/0, cannot_find_negative_change_values_test/0]).

-file("test/change_test.gleam", 5).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/change_test.gleam", 9).
-spec change_for_1_cent_test() -> {ok, list(integer())} |
    {error, change:error()}.
change_for_1_cent_test() ->
    _pipe = change:find_fewest_coins([1, 5, 10, 25], 1),
    exercism@should:equal(_pipe, {ok, [1]}).

-file("test/change_test.gleam", 14).
-spec single_coin_change_test() -> {ok, list(integer())} |
    {error, change:error()}.
single_coin_change_test() ->
    _pipe = change:find_fewest_coins([1, 5, 10, 25, 100], 25),
    exercism@should:equal(_pipe, {ok, [25]}).

-file("test/change_test.gleam", 19).
-spec multiple_coin_change_test() -> {ok, list(integer())} |
    {error, change:error()}.
multiple_coin_change_test() ->
    _pipe = change:find_fewest_coins([1, 5, 10, 25, 100], 15),
    exercism@should:equal(_pipe, {ok, [5, 10]}).

-file("test/change_test.gleam", 24).
-spec change_with_lilliputian_coins_test() -> {ok, list(integer())} |
    {error, change:error()}.
change_with_lilliputian_coins_test() ->
    _pipe = change:find_fewest_coins([1, 4, 15, 20, 50], 23),
    exercism@should:equal(_pipe, {ok, [4, 4, 15]}).

-file("test/change_test.gleam", 29).
-spec change_with_lower_elbonia_coins_test() -> {ok, list(integer())} |
    {error, change:error()}.
change_with_lower_elbonia_coins_test() ->
    _pipe = change:find_fewest_coins([1, 5, 10, 21, 25], 63),
    exercism@should:equal(_pipe, {ok, [21, 21, 21]}).

-file("test/change_test.gleam", 34).
-spec large_target_values_test() -> {ok, list(integer())} |
    {error, change:error()}.
large_target_values_test() ->
    _pipe = change:find_fewest_coins([1, 2, 5, 10, 20, 50, 100], 999),
    exercism@should:equal(
        _pipe,
        {ok, [2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100]}
    ).

-file("test/change_test.gleam", 41).
-spec possible_change_without_unit_coins_available_test() -> {ok,
        list(integer())} |
    {error, change:error()}.
possible_change_without_unit_coins_available_test() ->
    _pipe = change:find_fewest_coins([2, 5, 10, 20, 50], 21),
    exercism@should:equal(_pipe, {ok, [2, 2, 2, 5, 10]}).

-file("test/change_test.gleam", 46).
-spec another_possible_change_without_unit_coins_available_test() -> {ok,
        list(integer())} |
    {error, change:error()}.
another_possible_change_without_unit_coins_available_test() ->
    _pipe = change:find_fewest_coins([4, 5], 27),
    exercism@should:equal(_pipe, {ok, [4, 4, 4, 5, 5, 5]}).

-file("test/change_test.gleam", 51).
-spec no_coins_make_0_change_test() -> {ok, list(integer())} |
    {error, change:error()}.
no_coins_make_0_change_test() ->
    _pipe = change:find_fewest_coins([1, 5, 10, 21, 25], 0),
    exercism@should:equal(_pipe, {ok, []}).

-file("test/change_test.gleam", 56).
-spec error_testing_for_change_smaller_than_the_smallest_of_coins_test() -> {ok,
        list(integer())} |
    {error, change:error()}.
error_testing_for_change_smaller_than_the_smallest_of_coins_test() ->
    _pipe = change:find_fewest_coins([5, 10], 3),
    exercism@should:equal(_pipe, {error, impossible_target}).

-file("test/change_test.gleam", 61).
-spec error_if_no_combination_can_add_up_to_target_test() -> {ok,
        list(integer())} |
    {error, change:error()}.
error_if_no_combination_can_add_up_to_target_test() ->
    _pipe = change:find_fewest_coins([5, 10], 94),
    exercism@should:equal(_pipe, {error, impossible_target}).

-file("test/change_test.gleam", 66).
-spec cannot_find_negative_change_values_test() -> {ok, list(integer())} |
    {error, change:error()}.
cannot_find_negative_change_values_test() ->
    _pipe = change:find_fewest_coins([1, 2, 5], -5),
    exercism@should:equal(_pipe, {error, impossible_target}).
