-record(suite, {
    name :: binary(),
    path :: binary(),
    tests :: list(exercism_test_runner@internal:test())
}).
