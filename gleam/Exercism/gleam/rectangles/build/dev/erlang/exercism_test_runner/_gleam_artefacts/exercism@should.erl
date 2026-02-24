-module(exercism@should).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/exercism/should.gleam").
-export([equal/2]).

-file("src/exercism/should.gleam", 7).
-spec equal(KFT, KFT) -> KFT.
equal(A, B) ->
    case A =:= B of
        true ->
            A;

        false ->
            erlang:error(
                {unequal, gleam_stdlib:identity(A), gleam_stdlib:identity(B)}
            )
    end.
