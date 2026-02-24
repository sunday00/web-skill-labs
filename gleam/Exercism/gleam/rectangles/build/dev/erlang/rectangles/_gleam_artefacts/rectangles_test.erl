-module(rectangles_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/rectangles_test.gleam").
-export([main/0, no_rows_test/0, no_columns_test/0, no_rectangles_test/0, one_rectangle_test/0, two_rectangles_without_shared_parts_test/0, five_rectangles_with_shared_parts_test/0, rectangle_of_height_1_is_counted_test/0, rectangle_of_width_1_is_counted_test/0, one_by_one_square_is_counted_test/0, only_complete_rectangles_are_counted_test/0, rectangles_can_be_of_different_sizes_test/0, corner_is_required_for_a_rectangle_to_be_complete_test/0, large_input_with_many_rectangles_test/0, rectangles_must_have_four_sides_test/0]).

-file("test/rectangles_test.gleam", 5).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/rectangles_test.gleam", 9).
-spec no_rows_test() -> integer().
no_rows_test() ->
    _pipe = rectangles:rectangles(<<""/utf8>>),
    exercism@should:equal(_pipe, 0).

-file("test/rectangles_test.gleam", 14).
-spec no_columns_test() -> integer().
no_columns_test() ->
    _pipe = rectangles:rectangles(<<"
     
    "/utf8>>),
    exercism@should:equal(_pipe, 0).

-file("test/rectangles_test.gleam", 23).
-spec no_rectangles_test() -> integer().
no_rectangles_test() ->
    _pipe = rectangles:rectangles(<<" "/utf8>>),
    exercism@should:equal(_pipe, 0).

-file("test/rectangles_test.gleam", 28).
-spec one_rectangle_test() -> integer().
one_rectangle_test() ->
    _pipe = rectangles:rectangles(<<"
    +-+
    | |
    +-+
    "/utf8>>),
    exercism@should:equal(_pipe, 1).

-file("test/rectangles_test.gleam", 39).
-spec two_rectangles_without_shared_parts_test() -> integer().
two_rectangles_without_shared_parts_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +-+
      | |
    +-+-+
    | |  
    +-+  
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 2).

-file("test/rectangles_test.gleam", 52).
-spec five_rectangles_with_shared_parts_test() -> integer().
five_rectangles_with_shared_parts_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +-+
      | |
    +-+-+
    | | |
    +-+-+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 5).

-file("test/rectangles_test.gleam", 65).
-spec rectangle_of_height_1_is_counted_test() -> integer().
rectangle_of_height_1_is_counted_test() ->
    _pipe = rectangles:rectangles(<<"
    +--+
    +--+
    "/utf8>>),
    exercism@should:equal(_pipe, 1).

-file("test/rectangles_test.gleam", 75).
-spec rectangle_of_width_1_is_counted_test() -> integer().
rectangle_of_width_1_is_counted_test() ->
    _pipe = rectangles:rectangles(<<"
    ++
    ||
    ++
    "/utf8>>),
    exercism@should:equal(_pipe, 1).

-file("test/rectangles_test.gleam", 86).
-spec one_by_one_square_is_counted_test() -> integer().
one_by_one_square_is_counted_test() ->
    _pipe = rectangles:rectangles(<<"
    ++
    ++
    "/utf8>>),
    exercism@should:equal(_pipe, 1).

-file("test/rectangles_test.gleam", 96).
-spec only_complete_rectangles_are_counted_test() -> integer().
only_complete_rectangles_are_counted_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +-+
        |
    +-+-+
    | | -
    +-+-+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 1).

-file("test/rectangles_test.gleam", 109).
-spec rectangles_can_be_of_different_sizes_test() -> integer().
rectangles_can_be_of_different_sizes_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +------+----+
      |      |    |
      +---+--+    |
      |   |       |
      +---+-------+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 3).

-file("test/rectangles_test.gleam", 122).
-spec corner_is_required_for_a_rectangle_to_be_complete_test() -> integer().
corner_is_required_for_a_rectangle_to_be_complete_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +------+----+
      |      |    |
      +------+    |
      |   |       |
      +---+-------+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 2).

-file("test/rectangles_test.gleam", 135).
-spec large_input_with_many_rectangles_test() -> integer().
large_input_with_many_rectangles_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +---+--+----+
      |   +--+----+
      +---+--+    |
      |   +--+----+
      +---+--+--+-+
      +---+--+--+-+
      +------+  | |
                +-+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 60).

-file("test/rectangles_test.gleam", 151).
-spec rectangles_must_have_four_sides_test() -> integer().
rectangles_must_have_four_sides_test() ->
    _pipe = rectangles:rectangles(
        <<"
      +-+ +-+
      | | | |
      +-+-+-+
        | |  
      +-+-+-+
      | | | |
      +-+ +-+
    "/utf8>>
    ),
    exercism@should:equal(_pipe, 5).
