-module(gap@styled_comparison).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gap/styled_comparison.gleam").
-export_type([styled_comparison/0]).

-type styled_comparison() :: {styled_comparison, binary(), binary()}.


