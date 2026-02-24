-module(gap@comparison).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gap/comparison.gleam").
-export_type([comparison/1, match/1]).

-type comparison(FMJ) :: {list_comparison,
        list(match(list(FMJ))),
        list(match(list(FMJ)))} |
    {string_comparison,
        list(match(list(binary()))),
        list(match(list(binary())))}.

-type match(FMK) :: {match, FMK} | {no_match, FMK}.


