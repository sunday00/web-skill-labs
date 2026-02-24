-record(puzzle, {
    terms :: list(list(binary())),
    result_term :: list(binary()),
    unique_chars :: list(binary()),
    leading_chars :: gleam@set:set(binary()),
    max_length :: integer()
}).
