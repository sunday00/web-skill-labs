-module(gap).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/gap.gleam").
-export([to_styled/1, compare_strings_with_algorithm/3, compare_lists_with_algorithm/3, myers/2, compare_lists/2, compare_strings/2, lcs/2]).
-export_type([score/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type score(FVY) :: {score, integer(), gleam@option:option(FVY)}.

-file("src/gap.gleam", 41).
?DOC(
    " Creates a `StyledComparison` from `Comparison` using default values for\n"
    " highting and serialization.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " > compare_strings(\"abc\", \"abe\") |> to_styled()\n"
    " ```\n"
    " This will return a `StyledComparison(first, second)` where \"c\" in `first` is green\n"
    " and \"e\" in `second` is red.\n"
).
-spec to_styled(gap@comparison:comparison(any())) -> gap@styled_comparison:styled_comparison().
to_styled(Comparison) ->
    _pipe = Comparison,
    _pipe@1 = gap@styling:from_comparison(_pipe),
    _pipe@2 = gap@styling:highlight(
        _pipe@1,
        fun gap@styling:first_highlight_default/1,
        fun gap@styling:second_highlight_default/1,
        fun gap@styling:no_highlight/1
    ),
    gap@styling:to_styled_comparison(_pipe@2).

-file("src/gap.gleam", 65).
?DOC(
    " Compare two string and return a `StringComparison` which will be styled as string\n"
    " when passed to `to_styled`\n"
    " \n"
    " Algorithm can be used to select either `myers` or the legacy `lcs` algorithm\n"
).
-spec compare_strings_with_algorithm(
    binary(),
    binary(),
    fun((list(binary()), list(binary())) -> gap@comparison:comparison(binary()))
) -> gap@comparison:comparison(binary()).
compare_strings_with_algorithm(First, Second, Algorithm) ->
    Comparison = Algorithm(
        gleam@string:to_graphemes(First),
        gleam@string:to_graphemes(Second)
    ),
    case Comparison of
        {list_comparison, First@1, Second@1} ->
            {string_comparison, First@1, Second@1};

        {string_comparison, First@2, Second@2} ->
            {string_comparison, First@2, Second@2}
    end.

-file("src/gap.gleam", 93).
?DOC(
    " Compare two lists and return a `ListComparison` which will be styled as list\n"
    " when passed to `to_styled`\n"
    " \n"
    " Algorithm can be used to select either `myers` or the legacy `lcs` algorithm\n"
).
-spec compare_lists_with_algorithm(
    list(FWN),
    list(FWN),
    fun((list(FWN), list(FWN)) -> gap@comparison:comparison(FWN))
) -> gap@comparison:comparison(FWN).
compare_lists_with_algorithm(First_sequence, Second_sequence, Algorithm) ->
    Algorithm(First_sequence, Second_sequence).

-file("src/gap.gleam", 104).
?DOC(
    " An adapter for the the `myers` algorithm.\n"
    " Intended to be use as an argument to `compare_strings_with_algorithm` or\n"
    " `compare_lists_with_algorithm`\n"
).
-spec myers(list(FWS), list(FWS)) -> gap@comparison:comparison(FWS).
myers(First_sequence, Second_sequence) ->
    Edits = gap@myers:difference(First_sequence, Second_sequence),
    _pipe = Edits,
    _pipe@1 = lists:reverse(_pipe),
    gleam@list:fold(
        _pipe@1,
        {list_comparison, [], []},
        fun(Comparison, Edit) -> case Comparison of
                {list_comparison, First, Second} ->
                    case Edit of
                        {eq, Segment} ->
                            {list_comparison,
                                [{match, Segment} | First],
                                [{match, Segment} | Second]};

                        {ins, Segment@1} ->
                            {list_comparison,
                                First,
                                [{no_match, Segment@1} | Second]};

                        {del, Segment@2} ->
                            {list_comparison,
                                [{no_match, Segment@2} | First],
                                Second}
                    end;

                {string_comparison, _, _} ->
                    Comparison
            end end
    ).

-file("src/gap.gleam", 82).
?DOC(
    " Compare two lists and return a `ListComparison` which will be styled as list\n"
    " when passed to `to_styled`\n"
    " \n"
    " Will use the default `myers` algorithm\n"
).
-spec compare_lists(list(FWJ), list(FWJ)) -> gap@comparison:comparison(FWJ).
compare_lists(First_sequence, Second_sequence) ->
    myers(First_sequence, Second_sequence).

-file("src/gap.gleam", 52).
?DOC(
    " Compare two string and return a `StringComparison` which will be styled as string\n"
    " when passed to `to_styled`\n"
    " \n"
    " Will use the default `myers` algorithm\n"
).
-spec compare_strings(binary(), binary()) -> gap@comparison:comparison(binary()).
compare_strings(First, Second) ->
    Comparison = compare_lists(
        gleam@string:to_graphemes(First),
        gleam@string:to_graphemes(Second)
    ),
    case Comparison of
        {list_comparison, First@1, Second@1} ->
            {string_comparison, First@1, Second@1};

        {string_comparison, First@2, Second@2} ->
            {string_comparison, First@2, Second@2}
    end.

-file("src/gap.gleam", 235).
-spec prepend_and_merge(
    list(gap@comparison:match(list(FXA))),
    gap@comparison:match(list(FXA))
) -> list(gap@comparison:match(list(FXA))).
prepend_and_merge(Matches, Match) ->
    case {Matches, Match} of
        {[], _} ->
            [Match];

        {[{match, First_match} | Rest], {match, _}} ->
            [{match,
                    begin
                        _pipe = erlang:element(2, Match),
                        lists:append(_pipe, First_match)
                    end} |
                Rest];

        {[{no_match, First_match@1} | Rest@1], {no_match, _}} ->
            [{no_match,
                    begin
                        _pipe@1 = erlang:element(2, Match),
                        lists:append(_pipe@1, First_match@1)
                    end} |
                Rest@1];

        {Matches@1, Match@1} ->
            [Match@1 | Matches@1]
    end.

-file("src/gap.gleam", 259).
-spec append_and_merge(
    list(gap@comparison:match(list(FXJ))),
    gap@comparison:match(list(FXJ))
) -> list(gap@comparison:match(list(FXJ))).
append_and_merge(Matches, Match) ->
    _pipe@3 = case {begin
            _pipe = Matches,
            lists:reverse(_pipe)
        end,
        Match} of
        {[], _} ->
            [Match];

        {[{match, First_match} | Rest], {match, _}} ->
            [{match,
                    begin
                        _pipe@1 = First_match,
                        lists:append(_pipe@1, erlang:element(2, Match))
                    end} |
                Rest];

        {[{no_match, First_match@1} | Rest@1], {no_match, _}} ->
            [{no_match,
                    begin
                        _pipe@2 = First_match@1,
                        lists:append(_pipe@2, erlang:element(2, Match))
                    end} |
                Rest@1];

        {Matches@1, Match@1} ->
            [Match@1 | Matches@1]
    end,
    lists:reverse(_pipe@3).

-file("src/gap.gleam", 288).
-spec collect_matches(
    gleam@dict:dict({integer(), integer()}, any()),
    list(FXV),
    fun(({integer(), integer()}) -> integer())
) -> list(gap@comparison:match(list(FXV))).
collect_matches(Tracking, Str, Extract_fun) ->
    Matching_indexes = begin
        _pipe = maps:keys(Tracking),
        _pipe@1 = gleam@list:map(_pipe, Extract_fun),
        gleam@set:from_list(_pipe@1)
    end,
    Matches = begin
        _pipe@2 = Str,
        gleam@list:index_map(
            _pipe@2,
            fun(Item, Index) ->
                case gleam@set:contains(Matching_indexes, Index) of
                    true ->
                        {match, Item};

                    false ->
                        {no_match, Item}
                end
            end
        )
    end,
    _pipe@3 = Matches,
    _pipe@4 = gleam@list:chunk(_pipe@3, fun(Match) -> case Match of
                {match, _} ->
                    true;

                {no_match, _} ->
                    false
            end end),
    gleam@list:map(_pipe@4, fun(Match_list) -> case Match_list of
                [] ->
                    {match, []};

                [{match, _} | _] ->
                    {match,
                        gleam@list:filter_map(
                            Match_list,
                            fun(Match@1) -> case Match@1 of
                                    {match, Item@1} ->
                                        {ok, Item@1};

                                    {no_match, _} ->
                                        {error, nil}
                                end end
                        )};

                [{no_match, _} | _] ->
                    {no_match,
                        gleam@list:filter_map(
                            Match_list,
                            fun(Match@2) -> case Match@2 of
                                    {no_match, Item@2} ->
                                        {ok, Item@2};

                                    {match, _} ->
                                        {error, nil}
                                end end
                        )}
            end end).

-file("src/gap.gleam", 339).
-spec back_track(
    gleam@dict:dict({integer(), integer()}, score(FXY)),
    integer(),
    integer(),
    list({{integer(), integer()}, FXY})
) -> list({{integer(), integer()}, FXY}).
back_track(Diff_map, First_index, Second_index, Stack) ->
    case (First_index =:= 0) orelse (Second_index =:= 0) of
        true ->
            This_score = begin
                _pipe = gleam_stdlib:map_get(
                    Diff_map,
                    {First_index, Second_index}
                ),
                gleam@result:unwrap(_pipe, {score, 0, none})
            end,
            case This_score of
                {score, _, {some, Item}} ->
                    [{{First_index, Second_index}, Item} | Stack];

                _ ->
                    case {First_index, Second_index} of
                        {0, A} when A > 0 ->
                            back_track(
                                Diff_map,
                                First_index,
                                Second_index - 1,
                                Stack
                            );

                        {A@1, 0} when A@1 > 0 ->
                            back_track(
                                Diff_map,
                                First_index - 1,
                                Second_index,
                                Stack
                            );

                        {0, 0} ->
                            Stack;

                        {_, _} ->
                            back_track(
                                Diff_map,
                                First_index - 1,
                                Second_index,
                                Stack
                            )
                    end
            end;

        false ->
            This_score@1 = begin
                _pipe@1 = gleam_stdlib:map_get(
                    Diff_map,
                    {First_index, Second_index}
                ),
                gleam@result:unwrap(_pipe@1, {score, 0, none})
            end,
            case This_score@1 of
                {score, _, {some, Item@1}} ->
                    back_track(
                        Diff_map,
                        First_index - 1,
                        Second_index - 1,
                        [{{First_index, Second_index}, Item@1} | Stack]
                    );

                {score, _, none} ->
                    Up = begin
                        _pipe@2 = gleam_stdlib:map_get(
                            Diff_map,
                            {First_index, Second_index - 1}
                        ),
                        gleam@result:unwrap(_pipe@2, {score, 0, none})
                    end,
                    Back = begin
                        _pipe@3 = gleam_stdlib:map_get(
                            Diff_map,
                            {First_index - 1, Second_index}
                        ),
                        gleam@result:unwrap(_pipe@3, {score, 0, none})
                    end,
                    case gleam@int:compare(
                        erlang:element(2, Up),
                        erlang:element(2, Back)
                    ) of
                        gt ->
                            back_track(
                                Diff_map,
                                First_index,
                                Second_index - 1,
                                Stack
                            );

                        lt ->
                            back_track(
                                Diff_map,
                                First_index - 1,
                                Second_index,
                                Stack
                            );

                        eq ->
                            case {First_index, Second_index} of
                                {0, A@2} when A@2 > 0 ->
                                    back_track(
                                        Diff_map,
                                        First_index,
                                        Second_index - 1,
                                        Stack
                                    );

                                {A@3, 0} when A@3 > 0 ->
                                    back_track(
                                        Diff_map,
                                        First_index - 1,
                                        Second_index,
                                        Stack
                                    );

                                {0, 0} ->
                                    Stack;

                                {_, _} ->
                                    back_track(
                                        Diff_map,
                                        First_index - 1,
                                        Second_index,
                                        Stack
                                    )
                            end
                    end
            end
    end.

-file("src/gap.gleam", 400).
-spec build_diff_map(
    FYE,
    integer(),
    FYE,
    integer(),
    gleam@dict:dict({integer(), integer()}, score(FYE))
) -> gleam@dict:dict({integer(), integer()}, score(FYE)).
build_diff_map(First_item, First_index, Second_item, Second_index, Diff_map) ->
    Prev_score = begin
        _pipe = gleam_stdlib:map_get(
            Diff_map,
            {First_index - 1, Second_index - 1}
        ),
        gleam@result:unwrap(_pipe, {score, 0, none})
    end,
    Derived_score_up = begin
        _pipe@1 = Diff_map,
        _pipe@2 = gleam_stdlib:map_get(_pipe@1, {First_index, Second_index - 1}),
        gleam@result:unwrap(_pipe@2, {score, 0, none})
    end,
    Derived_score_back = begin
        _pipe@3 = Diff_map,
        _pipe@4 = gleam_stdlib:map_get(_pipe@3, {First_index - 1, Second_index}),
        gleam@result:unwrap(_pipe@4, {score, 0, none})
    end,
    Derived_score = gleam@int:max(
        erlang:element(2, Derived_score_up),
        erlang:element(2, Derived_score_back)
    ),
    This_score = case First_item =:= Second_item of
        true ->
            {score, erlang:element(2, Prev_score) + 1, {some, First_item}};

        false ->
            {score, Derived_score, none}
    end,
    _pipe@5 = Diff_map,
    gleam@dict:insert(_pipe@5, {First_index, Second_index}, This_score).

-file("src/gap.gleam", 132).
?DOC(
    " An adapter for the the `lcs` (longest common subsequence) algorithm.\n"
    " Intended to be use as an argument to `compare_strings_with_algorithm` or\n"
    " `compare_lists_with_algorithm`\n"
).
-spec lcs(list(FWW), list(FWW)) -> gap@comparison:comparison(FWW).
lcs(First_sequence, Second_sequence) ->
    Leading_matches = begin
        _pipe = gleam@list:zip(First_sequence, Second_sequence),
        _pipe@1 = gleam@list:take_while(
            _pipe,
            fun(Pair) -> erlang:element(1, Pair) =:= erlang:element(2, Pair) end
        ),
        gleam@list:map(_pipe@1, fun gleam@pair:first/1)
    end,
    Num_leading_matches = erlang:length(Leading_matches),
    Trailing_matches = begin
        _pipe@2 = gleam@list:zip(
            lists:reverse(First_sequence),
            lists:reverse(Second_sequence)
        ),
        _pipe@3 = gleam@list:take_while(
            _pipe@2,
            fun(Pair@1) ->
                erlang:element(1, Pair@1) =:= erlang:element(2, Pair@1)
            end
        ),
        _pipe@4 = gleam@list:map(_pipe@3, fun gleam@pair:first/1),
        lists:reverse(_pipe@4)
    end,
    Num_trailing_matches = erlang:length(Trailing_matches),
    First_sequence_to_diff = begin
        _pipe@5 = First_sequence,
        _pipe@6 = gleam@list:drop(_pipe@5, Num_leading_matches),
        gleam@list:take(
            _pipe@6,
            (erlang:length(First_sequence) - Num_leading_matches) - Num_trailing_matches
        )
    end,
    Second_sequence_to_diff = begin
        _pipe@7 = Second_sequence,
        _pipe@8 = gleam@list:drop(_pipe@7, Num_leading_matches),
        gleam@list:take(
            _pipe@8,
            (erlang:length(Second_sequence) - Num_leading_matches) - Num_trailing_matches
        )
    end,
    Diff_map@2 = begin
        _pipe@9 = Second_sequence_to_diff,
        gleam@list:index_fold(
            _pipe@9,
            maps:new(),
            fun(Diff_map, Item_second, Index_second) ->
                _pipe@10 = First_sequence_to_diff,
                gleam@list:index_fold(
                    _pipe@10,
                    Diff_map,
                    fun(Diff_map@1, Item_first, Index_first) ->
                        build_diff_map(
                            Item_first,
                            Index_first,
                            Item_second,
                            Index_second,
                            Diff_map@1
                        )
                    end
                )
            end
        )
    end,
    {First_segments@1, Second_segments@1} = case {First_sequence_to_diff,
        Second_sequence_to_diff} of
        {[], []} ->
            {[], []};

        {First_matching, []} ->
            {[{no_match, First_matching}], []};

        {[], Second_matching} ->
            {[], [{no_match, Second_matching}]};

        {First_sequence_to_diff@1, Second_sequence_to_diff@1} ->
            Tracking = begin
                _pipe@11 = back_track(
                    Diff_map@2,
                    erlang:length(First_sequence_to_diff@1) - 1,
                    erlang:length(Second_sequence_to_diff@1) - 1,
                    []
                ),
                maps:from_list(_pipe@11)
            end,
            First_segments = collect_matches(
                Tracking,
                First_sequence_to_diff@1,
                fun(Key) ->
                    {First, _} = Key,
                    First
                end
            ),
            Second_segments = collect_matches(
                Tracking,
                Second_sequence_to_diff@1,
                fun(Key@1) ->
                    {_, Second} = Key@1,
                    Second
                end
            ),
            {First_segments, Second_segments}
    end,
    {First_segments_with_leading_trailing,
        Second_segments_with_leading_trailing} = case {Leading_matches,
        Trailing_matches} of
        {[], []} ->
            {First_segments@1, Second_segments@1};

        {[], Trailing_matches@1} ->
            {begin
                    _pipe@12 = First_segments@1,
                    append_and_merge(_pipe@12, {match, Trailing_matches@1})
                end,
                begin
                    _pipe@13 = Second_segments@1,
                    append_and_merge(_pipe@13, {match, Trailing_matches@1})
                end};

        {Leading_matches@1, []} ->
            {begin
                    _pipe@14 = First_segments@1,
                    prepend_and_merge(_pipe@14, {match, Leading_matches@1})
                end,
                begin
                    _pipe@15 = Second_segments@1,
                    prepend_and_merge(_pipe@15, {match, Leading_matches@1})
                end};

        {Leading_matches@2, Trailing_matches@2} ->
            {begin
                    _pipe@16 = First_segments@1,
                    _pipe@17 = prepend_and_merge(
                        _pipe@16,
                        {match, Leading_matches@2}
                    ),
                    append_and_merge(_pipe@17, {match, Trailing_matches@2})
                end,
                begin
                    _pipe@18 = Second_segments@1,
                    _pipe@19 = prepend_and_merge(
                        _pipe@18,
                        {match, Leading_matches@2}
                    ),
                    append_and_merge(_pipe@19, {match, Trailing_matches@2})
                end}
    end,
    {list_comparison,
        First_segments_with_leading_trailing,
        Second_segments_with_leading_trailing}.
