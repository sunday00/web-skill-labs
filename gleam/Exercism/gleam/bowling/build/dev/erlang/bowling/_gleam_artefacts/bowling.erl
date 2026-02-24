-module(bowling).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/bowling.gleam").
-export([roll/2, score/1]).
-export_type([frame/0, game/0, error/0]).

-opaque frame() :: {frame, list(integer()), list(integer())}.

-type game() :: {game, list(frame())}.

-type error() :: invalid_pin_count | game_complete | game_not_complete.

-file("src/bowling.gleam", 20).
-spec roll(game(), integer()) -> {ok, game()} | {error, error()}.
roll(Game, Knocked_pins) ->
    case (Knocked_pins > 10) orelse (Knocked_pins < 0) of
        true ->
            {error, invalid_pin_count};

        false ->
            Rolled_current = case (Knocked_pins =:= 10) andalso (begin
                _pipe = erlang:element(2, Game),
                erlang:length(_pipe)
            end
            < 9) of
                true ->
                    [10, 0];

                false ->
                    [Knocked_pins]
            end,
            case begin
                _pipe@1 = erlang:element(2, Game),
                erlang:length(_pipe@1)
            end
            < 10 of
                true ->
                    case begin
                        _pipe@2 = erlang:element(2, Game),
                        gleam@list:last(_pipe@2)
                    end of
                        {ok, Last} ->
                            Old_last = begin
                                _pipe@3 = erlang:element(2, Game),
                                _pipe@4 = lists:reverse(_pipe@3),
                                _pipe@5 = gleam@list:drop(_pipe@4, 1),
                                gleam@list:first(_pipe@5)
                            end,
                            Strike_bonus = case Old_last of
                                {ok, Ol} ->
                                    case {erlang:element(2, Ol),
                                        begin
                                            _pipe@6 = erlang:element(2, Game),
                                            erlang:length(_pipe@6)
                                        end
                                        < 10} of
                                        {[10, 0], true} ->
                                            Knocked_pins;

                                        {_, _} ->
                                            0
                                    end;

                                {error, _} ->
                                    0
                            end,
                            case erlang:element(2, Last) of
                                [F, S] when ((F + S) =:= 10) andalso (F =/= 10) ->
                                    {ok,
                                        {game,
                                            begin
                                                _pipe@7 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                lists:append(
                                                    _pipe@7,
                                                    [{frame,
                                                            [Knocked_pins],
                                                            [Strike_bonus,
                                                                Knocked_pins]}]
                                                )
                                            end}};

                                [F@1, S@1] when (F@1 =:= 10) andalso (S@1 =:= 0) ->
                                    {ok,
                                        {game,
                                            begin
                                                _pipe@8 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                lists:append(
                                                    _pipe@8,
                                                    [{frame,
                                                            Rolled_current,
                                                            [Knocked_pins,
                                                                Strike_bonus]}]
                                                )
                                            end}};

                                [F@2] when (F@2 + Knocked_pins) > 10 ->
                                    {error, invalid_pin_count};

                                [F@3] when F@3 < 10 ->
                                    New_frames = begin
                                        _pipe@9 = erlang:element(2, Game),
                                        _pipe@10 = lists:reverse(_pipe@9),
                                        _pipe@11 = gleam@list:drop(_pipe@10, 1),
                                        _pipe@13 = gleam@list:prepend(
                                            _pipe@11,
                                            {frame,
                                                [F@3, Knocked_pins],
                                                begin
                                                    _pipe@12 = erlang:element(
                                                        3,
                                                        Last
                                                    ),
                                                    gleam@list:prepend(
                                                        _pipe@12,
                                                        Strike_bonus
                                                    )
                                                end}
                                        ),
                                        lists:reverse(_pipe@13)
                                    end,
                                    {ok, {game, New_frames}};

                                _ ->
                                    case (Knocked_pins =:= 10) andalso (begin
                                        _pipe@14 = erlang:element(2, Game),
                                        erlang:length(_pipe@14)
                                    end
                                    < 9) of
                                        true ->
                                            New_frames@1 = begin
                                                _pipe@15 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                lists:append(
                                                    _pipe@15,
                                                    [{frame,
                                                            [10, 0],
                                                            [Strike_bonus]}]
                                                )
                                            end,
                                            {ok, {game, New_frames@1}};

                                        false ->
                                            New_frames@2 = begin
                                                _pipe@16 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                lists:append(
                                                    _pipe@16,
                                                    [{frame,
                                                            [Knocked_pins],
                                                            [Strike_bonus]}]
                                                )
                                            end,
                                            {ok, {game, New_frames@2}}
                                    end
                            end;

                        {error, _} ->
                            Nf = {frame, Rolled_current, []},
                            {ok, {game, [Nf]}}
                    end;

                false ->
                    Old_last@1 = begin
                        _pipe@17 = erlang:element(2, Game),
                        _pipe@18 = lists:reverse(_pipe@17),
                        _pipe@19 = gleam@list:drop(_pipe@18, 1),
                        _pipe@20 = gleam@list:first(_pipe@19),
                        gleam@result:unwrap(_pipe@20, {frame, [], []})
                    end,
                    Ol_bonus = case erlang:element(2, Old_last@1) of
                        [10, 0] ->
                            Knocked_pins;

                        _ ->
                            0
                    end,
                    Last@1 = begin
                        _pipe@21 = erlang:element(2, Game),
                        _pipe@22 = gleam@list:last(_pipe@21),
                        gleam@result:unwrap(_pipe@22, {frame, [], []})
                    end,
                    case erlang:element(2, Last@1) of
                        [F@4] ->
                            case F@4 =:= 10 of
                                true ->
                                    New_frame = {frame,
                                        begin
                                            _pipe@23 = erlang:element(2, Last@1),
                                            lists:append(
                                                _pipe@23,
                                                [Knocked_pins]
                                            )
                                        end,
                                        begin
                                            _pipe@24 = erlang:element(3, Last@1),
                                            gleam@list:prepend(
                                                _pipe@24,
                                                Ol_bonus
                                            )
                                        end},
                                    {ok,
                                        {game,
                                            begin
                                                _pipe@25 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                _pipe@26 = lists:reverse(
                                                    _pipe@25
                                                ),
                                                _pipe@27 = gleam@list:drop(
                                                    _pipe@26,
                                                    1
                                                ),
                                                _pipe@28 = gleam@list:prepend(
                                                    _pipe@27,
                                                    New_frame
                                                ),
                                                lists:reverse(_pipe@28)
                                            end}};

                                false ->
                                    case (F@4 + Knocked_pins) > 10 of
                                        true ->
                                            {error, invalid_pin_count};

                                        false ->
                                            New_frame@1 = {frame,
                                                [F@4, Knocked_pins],
                                                begin
                                                    _pipe@29 = erlang:element(
                                                        3,
                                                        Last@1
                                                    ),
                                                    gleam@list:prepend(
                                                        _pipe@29,
                                                        Ol_bonus
                                                    )
                                                end},
                                            {ok,
                                                {game,
                                                    begin
                                                        _pipe@30 = erlang:element(
                                                            2,
                                                            Game
                                                        ),
                                                        _pipe@31 = lists:reverse(
                                                            _pipe@30
                                                        ),
                                                        _pipe@32 = gleam@list:drop(
                                                            _pipe@31,
                                                            1
                                                        ),
                                                        _pipe@33 = gleam@list:prepend(
                                                            _pipe@32,
                                                            New_frame@1
                                                        ),
                                                        lists:reverse(_pipe@33)
                                                    end}}
                                    end
                            end;

                        [F@5, S@2] ->
                            case {F@5, S@2} of
                                {F@6, S@3} when (F@6 =:= 10) andalso (S@3 =:= 10) ->
                                    New_frame@2 = {frame,
                                        [10, 10, Knocked_pins],
                                        erlang:element(3, Last@1)},
                                    {ok,
                                        {game,
                                            begin
                                                _pipe@34 = erlang:element(
                                                    2,
                                                    Game
                                                ),
                                                _pipe@35 = lists:reverse(
                                                    _pipe@34
                                                ),
                                                _pipe@36 = gleam@list:drop(
                                                    _pipe@35,
                                                    1
                                                ),
                                                _pipe@37 = gleam@list:prepend(
                                                    _pipe@36,
                                                    New_frame@2
                                                ),
                                                lists:reverse(_pipe@37)
                                            end}};

                                {F@7, S@4} when (F@7 =:= 10) andalso (S@4 < 10) ->
                                    case (S@4 + Knocked_pins) > 10 of
                                        true ->
                                            {error, invalid_pin_count};

                                        false ->
                                            New_frame@3 = {frame,
                                                [10, S@4, Knocked_pins],
                                                erlang:element(3, Last@1)},
                                            {ok,
                                                {game,
                                                    begin
                                                        _pipe@38 = erlang:element(
                                                            2,
                                                            Game
                                                        ),
                                                        _pipe@39 = lists:reverse(
                                                            _pipe@38
                                                        ),
                                                        _pipe@40 = gleam@list:drop(
                                                            _pipe@39,
                                                            1
                                                        ),
                                                        _pipe@41 = gleam@list:prepend(
                                                            _pipe@40,
                                                            New_frame@3
                                                        ),
                                                        lists:reverse(_pipe@41)
                                                    end}}
                                    end;

                                {F@8, S@5} when (F@8 < 10) andalso (S@5 =:= 10) ->
                                    {error, invalid_pin_count};

                                {F@9, S@6} when (F@9 < 10) andalso (S@6 < 10) ->
                                    case (F@9 + S@6) =:= 10 of
                                        true ->
                                            New_frame@4 = {frame,
                                                [F@9, S@6, Knocked_pins],
                                                erlang:element(3, Last@1)},
                                            {ok,
                                                {game,
                                                    begin
                                                        _pipe@42 = erlang:element(
                                                            2,
                                                            Game
                                                        ),
                                                        _pipe@43 = lists:reverse(
                                                            _pipe@42
                                                        ),
                                                        _pipe@44 = gleam@list:drop(
                                                            _pipe@43,
                                                            1
                                                        ),
                                                        _pipe@45 = gleam@list:prepend(
                                                            _pipe@44,
                                                            New_frame@4
                                                        ),
                                                        lists:reverse(_pipe@45)
                                                    end}};

                                        false ->
                                            {error, game_complete}
                                    end;

                                {_, _} ->
                                    {error, invalid_pin_count}
                            end;

                        [F@10, S@7, T] ->
                            {error, game_complete};

                        _ ->
                            erlang:error(#{gleam_error => todo,
                                    message => <<"`todo` expression evaluated. This code has not yet been implemented."/utf8>>,
                                    file => <<?FILEPATH/utf8>>,
                                    module => <<"bowling"/utf8>>,
                                    function => <<"roll"/utf8>>,
                                    line => 209})
                    end
            end
    end.

-file("src/bowling.gleam", 218).
-spec score(game()) -> {ok, integer()} | {error, error()}.
score(Game) ->
    case begin
        _pipe = erlang:element(2, Game),
        erlang:length(_pipe)
    end of
        L when L < 10 ->
            {error, game_not_complete};

        _ ->
            Last = begin
                _pipe@1 = erlang:element(2, Game),
                _pipe@2 = gleam@list:last(_pipe@1),
                gleam@result:unwrap(_pipe@2, {frame, [], []})
            end,
            case erlang:element(2, Last) of
                [F] when F =:= 10 ->
                    {error, game_not_complete};

                [F@1, S] when (F@1 + S) =:= 10 ->
                    {error, game_not_complete};

                [F@2, S@1] when (F@2 + S@1) =:= 20 ->
                    {error, game_not_complete};

                _ ->
                    _pipe@3 = erlang:element(2, Game),
                    _pipe@6 = gleam@list:fold(
                        _pipe@3,
                        0,
                        fun(Acc, Cur) ->
                            (Acc + begin
                                _pipe@4 = erlang:element(2, Cur),
                                gleam@list:fold(
                                    _pipe@4,
                                    0,
                                    fun(Ac, Cu) -> Ac + Cu end
                                )
                            end)
                            + begin
                                _pipe@5 = erlang:element(3, Cur),
                                gleam@list:fold(
                                    _pipe@5,
                                    0,
                                    fun(Ac@1, Cu@1) -> Ac@1 + Cu@1 end
                                )
                            end
                        end
                    ),
                    {ok, _pipe@6}
            end
    end.
