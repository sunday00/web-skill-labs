-module(gap@comparison).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([comparison/1, match/1]).

-type comparison(FYP) :: {list_comparison,
        list(match(list(FYP))),
        list(match(list(FYP)))} |
    {string_comparison,
        list(match(list(binary()))),
        list(match(list(binary())))}.

-type match(FYQ) :: {match, FYQ} | {no_match, FYQ}.


