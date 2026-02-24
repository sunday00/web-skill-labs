-record(test_result, {
    the_test :: exercism_test_runner@internal:test(),
    error :: gleam@option:option(exercism_test_runner@internal:error()),
    output :: binary()
}).
