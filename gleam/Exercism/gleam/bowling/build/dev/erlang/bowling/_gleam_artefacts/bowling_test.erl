-module(bowling_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/bowling_test.gleam").
-export([main/0, negative_point_roll_test/0, more_than_10_points_roll_test/0, more_than_10_points_frame_test/0, trying_to_score_unstarted_game_test/0, all_zeroes_score_test/0, no_strikes_or_spares_test/0, spare_followed_by_zeroes_test/0, points_after_spare_test/0, consecutive_spares_test/0, spare_in_last_frame_test/0, strike_in_single_roll_frame_test/0, two_rolls_after_strike_test/0, consecutive_strikes_test/0, strike_in_last_frame_test/0, spare_with_two_roll_bonus_test/0, strike_with_two_roll_bonus_test/0, strike_with_one_roll_bonus_after_spare_in_last_frame_test/0, all_strikes_test/0, two_bonus_rolls_after_strike_in_last_frame_and_one_is_strike_test/0, last_two_strikes_followed_by_only_last_bonus_non_strike_points_test/0, bonus_roll_after_strike_in_last_frame_test/0, two_bonus_rolls_after_strike_in_last_frame_test/0, two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_not_strike_test/0, two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_strike_test/0, rolling_after_10_frames_test/0, rolling_after_bonus_roll_after_spare_test/0, rolling_after_bonus_rolls_after_strike_test/0, trying_to_score_incomplete_game_test/0, trying_to_score_game_before_rolling_bonus_rolls_after_strike_in_last_frame_test/0, trying_to_score_game_before_rolling_both_bonus_rolls_after_strike_in_last_frame_test/0, trying_to_score_game_before_rolling_bonus_roll_after_spare_in_last_frame_test/0]).

-file("test/bowling_test.gleam", 8).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/bowling_test.gleam", 110).
-spec negative_point_roll_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
negative_point_roll_test() ->
    _assert_subject = bowling:roll({game, []}, -1),
    case _assert_subject of
        {error, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"bowling_test"/utf8>>,
                        function => <<"negative_point_roll_test"/utf8>>,
                        line => 111,
                        value => _assert_fail,
                        start => 2405,
                        'end' => 2445,
                        pattern_start => 2416,
                        pattern_end => 2424})
    end.

-file("test/bowling_test.gleam", 114).
-spec more_than_10_points_roll_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
more_than_10_points_roll_test() ->
    _assert_subject = bowling:roll({game, []}, 11),
    case _assert_subject of
        {error, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"bowling_test"/utf8>>,
                        function => <<"more_than_10_points_roll_test"/utf8>>,
                        line => 115,
                        value => _assert_fail,
                        start => 2492,
                        'end' => 2532,
                        pattern_start => 2503,
                        pattern_end => 2511})
    end.

-file("test/bowling_test.gleam", 118).
-spec more_than_10_points_frame_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
more_than_10_points_frame_test() ->
    Game@1 = case bowling:roll({game, []}, 5) of
        {ok, Game} -> Game;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"bowling_test"/utf8>>,
                        function => <<"more_than_10_points_frame_test"/utf8>>,
                        line => 119,
                        value => _assert_fail,
                        start => 2580,
                        'end' => 2619,
                        pattern_start => 2591,
                        pattern_end => 2599})
    end,
    _assert_subject = bowling:roll(Game@1, 6),
    case _assert_subject of
        {error, _} -> _assert_subject;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"bowling_test"/utf8>>,
                        function => <<"more_than_10_points_frame_test"/utf8>>,
                        line => 120,
                        value => _assert_fail@1,
                        start => 2622,
                        'end' => 2657,
                        pattern_start => 2633,
                        pattern_end => 2641})
    end.

-file("test/bowling_test.gleam", 158).
-spec trying_to_score_unstarted_game_test() -> {ok, integer()} |
    {error, bowling:error()}.
trying_to_score_unstarted_game_test() ->
    _assert_subject = bowling:score({game, []}),
    case _assert_subject of
        {error, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"bowling_test"/utf8>>,
                        function => <<"trying_to_score_unstarted_game_test"/utf8>>,
                        line => 159,
                        value => _assert_fail,
                        start => 3757,
                        'end' => 3794,
                        pattern_start => 3768,
                        pattern_end => 3776})
    end.

-file("test/bowling_test.gleam", 218).
-spec roll_and_check_score(list(integer()), integer()) -> {ok, integer()} |
    {error, bowling:error()}.
roll_and_check_score(Rolls, Correct_score) ->
    _pipe = Rolls,
    _pipe@1 = gleam@list:fold(
        _pipe,
        {game, []},
        fun(Game, Pins) ->
            New_game@1 = case bowling:roll(Game, Pins) of
                {ok, New_game} -> New_game;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"bowling_test"/utf8>>,
                                function => <<"roll_and_check_score"/utf8>>,
                                line => 221,
                                value => _assert_fail,
                                start => 5422,
                                'end' => 5464,
                                pattern_start => 5433,
                                pattern_end => 5445})
            end,
            New_game@1
        end
    ),
    _pipe@2 = bowling:score(_pipe@1),
    exercism@should:equal(_pipe@2, {ok, Correct_score}).

-file("test/bowling_test.gleam", 12).
-spec all_zeroes_score_test() -> {ok, integer()} | {error, bowling:error()}.
all_zeroes_score_test() ->
    Rolls = gleam@list:repeat(0, 20),
    _pipe = Rolls,
    roll_and_check_score(_pipe, 0).

-file("test/bowling_test.gleam", 19).
-spec no_strikes_or_spares_test() -> {ok, integer()} | {error, bowling:error()}.
no_strikes_or_spares_test() ->
    Rolls = [3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6, 3, 6],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 90).

-file("test/bowling_test.gleam", 26).
-spec spare_followed_by_zeroes_test() -> {ok, integer()} |
    {error, bowling:error()}.
spare_followed_by_zeroes_test() ->
    Rolls = [6, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 10).

-file("test/bowling_test.gleam", 33).
-spec points_after_spare_test() -> {ok, integer()} | {error, bowling:error()}.
points_after_spare_test() ->
    Rolls = [6, 4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 16).

-file("test/bowling_test.gleam", 40).
-spec consecutive_spares_test() -> {ok, integer()} | {error, bowling:error()}.
consecutive_spares_test() ->
    Rolls = [5, 5, 3, 7, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 31).

-file("test/bowling_test.gleam", 47).
-spec spare_in_last_frame_test() -> {ok, integer()} | {error, bowling:error()}.
spare_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 7],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 17).

-file("test/bowling_test.gleam", 54).
-spec strike_in_single_roll_frame_test() -> {ok, integer()} |
    {error, bowling:error()}.
strike_in_single_roll_frame_test() ->
    Rolls = [10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 10).

-file("test/bowling_test.gleam", 61).
-spec two_rolls_after_strike_test() -> {ok, integer()} |
    {error, bowling:error()}.
two_rolls_after_strike_test() ->
    Rolls = [10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 26).

-file("test/bowling_test.gleam", 68).
-spec consecutive_strikes_test() -> {ok, integer()} | {error, bowling:error()}.
consecutive_strikes_test() ->
    Rolls = [10, 10, 10, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 81).

-file("test/bowling_test.gleam", 75).
-spec strike_in_last_frame_test() -> {ok, integer()} | {error, bowling:error()}.
strike_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 1],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 18).

-file("test/bowling_test.gleam", 82).
-spec spare_with_two_roll_bonus_test() -> {ok, integer()} |
    {error, bowling:error()}.
spare_with_two_roll_bonus_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 7, 3],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 20).

-file("test/bowling_test.gleam", 89).
-spec strike_with_two_roll_bonus_test() -> {ok, integer()} |
    {error, bowling:error()}.
strike_with_two_roll_bonus_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 10],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 30).

-file("test/bowling_test.gleam", 96).
-spec strike_with_one_roll_bonus_after_spare_in_last_frame_test() -> {ok,
        integer()} |
    {error, bowling:error()}.
strike_with_one_roll_bonus_after_spare_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 10],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 20).

-file("test/bowling_test.gleam", 103).
-spec all_strikes_test() -> {ok, integer()} | {error, bowling:error()}.
all_strikes_test() ->
    Rolls = [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 300).

-file("test/bowling_test.gleam", 137).
-spec two_bonus_rolls_after_strike_in_last_frame_and_one_is_strike_test() -> {ok,
        integer()} |
    {error, bowling:error()}.
two_bonus_rolls_after_strike_in_last_frame_and_one_is_strike_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 6],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 26).

-file("test/bowling_test.gleam", 211).
-spec last_two_strikes_followed_by_only_last_bonus_non_strike_points_test() -> {ok,
        integer()} |
    {error, bowling:error()}.
last_two_strikes_followed_by_only_last_bonus_non_strike_points_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10, 0, 1],
    _pipe = Rolls,
    roll_and_check_score(_pipe, 31).

-file("test/bowling_test.gleam", 228).
-spec roll_and_last_roll_be_error(list(integer()), integer(), bowling:error()) -> {ok,
        bowling:game()} |
    {error, bowling:error()}.
roll_and_last_roll_be_error(Rolls, Last_roll, Error) ->
    _pipe = Rolls,
    _pipe@1 = gleam@list:fold(
        _pipe,
        {game, []},
        fun(Game, Pins) ->
            New_game@1 = case bowling:roll(Game, Pins) of
                {ok, New_game} -> New_game;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"bowling_test"/utf8>>,
                                function => <<"roll_and_last_roll_be_error"/utf8>>,
                                line => 231,
                                value => _assert_fail,
                                start => 5671,
                                'end' => 5713,
                                pattern_start => 5682,
                                pattern_end => 5694})
            end,
            New_game@1
        end
    ),
    _pipe@2 = bowling:roll(_pipe@1, Last_roll),
    exercism@should:equal(_pipe@2, {error, Error}).

-file("test/bowling_test.gleam", 123).
-spec bonus_roll_after_strike_in_last_frame_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
bonus_roll_after_strike_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 11, invalid_pin_count).

-file("test/bowling_test.gleam", 130).
-spec two_bonus_rolls_after_strike_in_last_frame_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
two_bonus_rolls_after_strike_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 5],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 6, invalid_pin_count).

-file("test/bowling_test.gleam", 144).
-spec two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_not_strike_test(
    
) -> {ok, bowling:game()} | {error, bowling:error()}.
two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_not_strike_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 6],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 10, invalid_pin_count).

-file("test/bowling_test.gleam", 151).
-spec two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_strike_test() -> {ok,
        bowling:game()} |
    {error, bowling:error()}.
two_bonus_rolls_after_strike_in_last_frame_and_first_one_is_strike_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 11, invalid_pin_count).

-file("test/bowling_test.gleam", 169).
-spec rolling_after_10_frames_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
rolling_after_10_frames_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 0, game_complete).

-file("test/bowling_test.gleam", 197).
-spec rolling_after_bonus_roll_after_spare_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
rolling_after_bonus_roll_after_spare_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3, 2],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 2, game_complete).

-file("test/bowling_test.gleam", 204).
-spec rolling_after_bonus_rolls_after_strike_test() -> {ok, bowling:game()} |
    {error, bowling:error()}.
rolling_after_bonus_rolls_after_strike_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 2],
    _pipe = Rolls,
    roll_and_last_roll_be_error(_pipe, 2, game_complete).

-file("test/bowling_test.gleam", 238).
-spec roll_and_score_be_error(list(integer())) -> {ok, integer()} |
    {error, bowling:error()}.
roll_and_score_be_error(Rolls) ->
    _pipe = Rolls,
    _pipe@1 = gleam@list:fold(
        _pipe,
        {game, []},
        fun(Game, Pins) ->
            New_game@1 = case bowling:roll(Game, Pins) of
                {ok, New_game} -> New_game;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"bowling_test"/utf8>>,
                                function => <<"roll_and_score_be_error"/utf8>>,
                                line => 241,
                                value => _assert_fail,
                                start => 5889,
                                'end' => 5931,
                                pattern_start => 5900,
                                pattern_end => 5912})
            end,
            New_game@1
        end
    ),
    _pipe@2 = bowling:score(_pipe@1),
    exercism@should:equal(_pipe@2, {error, game_not_complete}).

-file("test/bowling_test.gleam", 162).
-spec trying_to_score_incomplete_game_test() -> {ok, integer()} |
    {error, bowling:error()}.
trying_to_score_incomplete_game_test() ->
    Rolls = [0, 0],
    _pipe = Rolls,
    roll_and_score_be_error(_pipe).

-file("test/bowling_test.gleam", 176).
-spec trying_to_score_game_before_rolling_bonus_rolls_after_strike_in_last_frame_test(
    
) -> {ok, integer()} | {error, bowling:error()}.
trying_to_score_game_before_rolling_bonus_rolls_after_strike_in_last_frame_test(
    
) ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10],
    _pipe = Rolls,
    roll_and_score_be_error(_pipe).

-file("test/bowling_test.gleam", 183).
-spec trying_to_score_game_before_rolling_both_bonus_rolls_after_strike_in_last_frame_test(
    
) -> {ok, integer()} | {error, bowling:error()}.
trying_to_score_game_before_rolling_both_bonus_rolls_after_strike_in_last_frame_test(
    
) ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 10],
    _pipe = Rolls,
    roll_and_score_be_error(_pipe).

-file("test/bowling_test.gleam", 190).
-spec trying_to_score_game_before_rolling_bonus_roll_after_spare_in_last_frame_test(
    
) -> {ok, integer()} | {error, bowling:error()}.
trying_to_score_game_before_rolling_bonus_roll_after_spare_in_last_frame_test() ->
    Rolls = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 3],
    _pipe = Rolls,
    roll_and_score_be_error(_pipe).
