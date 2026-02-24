-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gleam/function.gleam").
-export([flip/1, identity/1, tap/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/gleam/function.gleam", 5).
?DOC(
    " Takes a function that takes two arguments and returns a new function that\n"
    " takes the same two arguments, but in reverse order.\n"
).
-spec flip(fun((EFC, EFD) -> EFE)) -> fun((EFD, EFC) -> EFE).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-file("src/gleam/function.gleam", 11).
?DOC(" Takes a single argument and always returns its input value.\n").
-spec identity(EFF) -> EFF.
identity(X) ->
    X.

-file("src/gleam/function.gleam", 20).
?DOC(
    " Takes an argument and a single function,\n"
    " calls that function with that argument\n"
    " and returns that argument instead of the function return value.\n"
    " Useful for running synchronous side effects in a pipeline.\n"
).
-spec tap(EFG, fun((EFG) -> any())) -> EFG.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.
