-module(variable_length_quantity_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/variable_length_quantity_test.gleam").
-export([main/0, encode_a_series_of_integers_producing_a_series_of_bytes_zero_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_single_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_largest_single_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_smallest_double_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_double_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_largest_double_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_smallest_triple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_triple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_largest_triple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quadruple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quadruple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_largest_quadruple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quintuple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quintuple_byte_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_maximum_32_bit_integer_input_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_two_single_byte_values_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_two_multi_byte_values_test/0, encode_a_series_of_integers_producing_a_series_of_bytes_many_multi_byte_values_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_one_byte_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_two_bytes_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_three_bytes_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_four_bytes_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_maximum_32_bit_integer_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_even_if_value_is_zero_test/0, decode_a_series_of_bytes_producing_a_series_of_integers_multiple_values_test/0]).

-file("test/variable_length_quantity_test.gleam", 5).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/variable_length_quantity_test.gleam", 9).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_zero_test() -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_zero_test() ->
    _pipe = variable_length_quantity:encode([16#0]),
    exercism@should:equal(_pipe, <<16#0>>).

-file("test/variable_length_quantity_test.gleam", 14).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_single_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_single_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#40]),
    exercism@should:equal(_pipe, <<16#40>>).

-file("test/variable_length_quantity_test.gleam", 19).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_largest_single_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_largest_single_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#7F]),
    exercism@should:equal(_pipe, <<16#7F>>).

-file("test/variable_length_quantity_test.gleam", 24).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_smallest_double_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_smallest_double_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#80]),
    exercism@should:equal(_pipe, <<16#81, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 29).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_double_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_double_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#2000]),
    exercism@should:equal(_pipe, <<16#C0, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 34).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_largest_double_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_largest_double_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#3FFF]),
    exercism@should:equal(_pipe, <<16#FF, 16#7F>>).

-file("test/variable_length_quantity_test.gleam", 39).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_smallest_triple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_smallest_triple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#4000]),
    exercism@should:equal(_pipe, <<16#81, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 44).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_triple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_triple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#100000]),
    exercism@should:equal(_pipe, <<16#C0, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 49).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_largest_triple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_largest_triple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#1FFFFF]),
    exercism@should:equal(_pipe, <<16#FF, 16#FF, 16#7F>>).

-file("test/variable_length_quantity_test.gleam", 54).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quadruple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quadruple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#200000]),
    exercism@should:equal(_pipe, <<16#81, 16#80, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 59).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quadruple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quadruple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#8000000]),
    exercism@should:equal(_pipe, <<16#C0, 16#80, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 64).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_largest_quadruple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_largest_quadruple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#FFFFFFF]),
    exercism@should:equal(_pipe, <<16#FF, 16#FF, 16#FF, 16#7F>>).

-file("test/variable_length_quantity_test.gleam", 69).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quintuple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_smallest_quintuple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#10000000]),
    exercism@should:equal(_pipe, <<16#81, 16#80, 16#80, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 74).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quintuple_byte_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_arbitrary_quintuple_byte_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#FF000000]),
    exercism@should:equal(_pipe, <<16#8F, 16#F8, 16#80, 16#80, 16#0>>).

-file("test/variable_length_quantity_test.gleam", 79).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_maximum_32_bit_integer_input_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_maximum_32_bit_integer_input_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#FFFFFFFF]),
    exercism@should:equal(_pipe, <<16#8F, 16#FF, 16#FF, 16#FF, 16#7F>>).

-file("test/variable_length_quantity_test.gleam", 84).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_two_single_byte_values_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_two_single_byte_values_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#40, 16#7F]),
    exercism@should:equal(_pipe, <<16#40, 16#7F>>).

-file("test/variable_length_quantity_test.gleam", 89).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_two_multi_byte_values_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_two_multi_byte_values_test(
    
) ->
    _pipe = variable_length_quantity:encode([16#4000, 16#123456]),
    exercism@should:equal(_pipe, <<16#81, 16#80, 16#0, 16#C8, 16#E8, 16#56>>).

-file("test/variable_length_quantity_test.gleam", 94).
-spec encode_a_series_of_integers_producing_a_series_of_bytes_many_multi_byte_values_test(
    
) -> bitstring().
encode_a_series_of_integers_producing_a_series_of_bytes_many_multi_byte_values_test(
    
) ->
    _pipe = variable_length_quantity:encode(
        [16#2000, 16#123456, 16#FFFFFFF, 16#0, 16#3FFF, 16#4000]
    ),
    exercism@should:equal(
        _pipe,
        <<16#C0,
            16#0,
            16#C8,
            16#E8,
            16#56,
            16#FF,
            16#FF,
            16#FF,
            16#7F,
            16#0,
            16#FF,
            16#7F,
            16#81,
            16#80,
            16#0>>
    ).

-file("test/variable_length_quantity_test.gleam", 104).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_one_byte_test() -> {ok,
        list(integer())} |
    {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_one_byte_test() ->
    _pipe = variable_length_quantity:decode(<<16#7F>>),
    exercism@should:equal(_pipe, {ok, [16#7F]}).

-file("test/variable_length_quantity_test.gleam", 109).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_two_bytes_test() -> {ok,
        list(integer())} |
    {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_two_bytes_test() ->
    _pipe = variable_length_quantity:decode(<<16#C0, 16#0>>),
    exercism@should:equal(_pipe, {ok, [16#2000]}).

-file("test/variable_length_quantity_test.gleam", 114).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_three_bytes_test() -> {ok,
        list(integer())} |
    {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_three_bytes_test() ->
    _pipe = variable_length_quantity:decode(<<16#FF, 16#FF, 16#7F>>),
    exercism@should:equal(_pipe, {ok, [16#1FFFFF]}).

-file("test/variable_length_quantity_test.gleam", 119).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_four_bytes_test() -> {ok,
        list(integer())} |
    {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_four_bytes_test() ->
    _pipe = variable_length_quantity:decode(<<16#81, 16#80, 16#80, 16#0>>),
    exercism@should:equal(_pipe, {ok, [16#200000]}).

-file("test/variable_length_quantity_test.gleam", 124).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_maximum_32_bit_integer_test(
    
) -> {ok, list(integer())} | {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_maximum_32_bit_integer_test(
    
) ->
    _pipe = variable_length_quantity:decode(
        <<16#8F, 16#FF, 16#FF, 16#FF, 16#7F>>
    ),
    exercism@should:equal(_pipe, {ok, [16#FFFFFFFF]}).

-file("test/variable_length_quantity_test.gleam", 129).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_test(
    
) -> {ok, list(integer())} | {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_test(
    
) ->
    _pipe = variable_length_quantity:decode(<<16#FF>>),
    exercism@should:equal(_pipe, {error, incomplete_sequence}).

-file("test/variable_length_quantity_test.gleam", 134).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_even_if_value_is_zero_test(
    
) -> {ok, list(integer())} | {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_incomplete_sequence_causes_error_even_if_value_is_zero_test(
    
) ->
    _pipe = variable_length_quantity:decode(<<16#80>>),
    exercism@should:equal(_pipe, {error, incomplete_sequence}).

-file("test/variable_length_quantity_test.gleam", 139).
-spec decode_a_series_of_bytes_producing_a_series_of_integers_multiple_values_test(
    
) -> {ok, list(integer())} | {error, variable_length_quantity:error()}.
decode_a_series_of_bytes_producing_a_series_of_integers_multiple_values_test() ->
    _pipe = variable_length_quantity:decode(
        <<16#C0,
            16#0,
            16#C8,
            16#E8,
            16#56,
            16#FF,
            16#FF,
            16#FF,
            16#7F,
            16#0,
            16#FF,
            16#7F,
            16#81,
            16#80,
            16#0>>
    ),
    exercism@should:equal(
        _pipe,
        {ok, [16#2000, 16#123456, 16#FFFFFFF, 16#0, 16#3FFF, 16#4000]}
    ).
