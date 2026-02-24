-record(test, {
    name :: binary(),
    module_path :: binary(),
    function :: fun(() -> {ok, nil} |
        {error, exercism_test_runner@internal:error()}),
    src :: binary()
}).
