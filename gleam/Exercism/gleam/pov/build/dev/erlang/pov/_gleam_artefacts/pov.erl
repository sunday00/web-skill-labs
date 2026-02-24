-module(pov).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/pov.gleam").
-export([from_pov/2, path_to/3, main/0]).
-export_type([tree/1]).

-type tree(ARD) :: {tree, ARD, list(tree(ARD))}.

-file("src/pov.gleam", 52).
-spec make_map(
    {gleam@dict:dict(ARO, list(ARO)), gleam@dict:dict(ARO, ARO)},
    tree(ARO)
) -> {gleam@dict:dict(ARO, list(ARO)), gleam@dict:dict(ARO, ARO)}.
make_map(Acc, Tree) ->
    Children = begin
        _pipe = erlang:element(3, Tree),
        gleam@list:map(_pipe, fun(El) -> erlang:element(2, El) end)
    end,
    Acc_0 = begin
        _pipe@1 = erlang:element(1, Acc),
        gleam@dict:insert(_pipe@1, erlang:element(2, Tree), Children)
    end,
    Acc_1 = begin
        _pipe@2 = erlang:element(3, Tree),
        gleam@list:fold(
            _pipe@2,
            erlang:element(2, Acc),
            fun(Ac, El@1) -> _pipe@3 = Ac,
                gleam@dict:insert(
                    _pipe@3,
                    erlang:element(2, El@1),
                    erlang:element(2, Tree)
                ) end
        )
    end,
    case begin
        _pipe@4 = erlang:element(3, Tree),
        erlang:length(_pipe@4)
    end
    > 0 of
        true ->
            _pipe@5 = erlang:element(3, Tree),
            gleam@list:fold(
                _pipe@5,
                {Acc_0, Acc_1},
                fun(Acc@1, El@2) -> make_map(Acc@1, El@2) end
            );

        false ->
            {begin
                    _pipe@6 = erlang:element(1, Acc),
                    gleam@dict:insert(_pipe@6, erlang:element(2, Tree), [])
                end,
                erlang:element(2, Acc)}
    end.

-file("src/pov.gleam", 70).
-spec filter_map1(gleam@dict:dict(ARW, ARW), gleam@dict:dict(ARW, ARW), ARW) -> gleam@dict:dict(ARW, ARW).
filter_map1(Acc, Map, Tree) ->
    case begin
        _pipe = Map,
        gleam_stdlib:map_get(_pipe, Tree)
    end of
        {ok, P} ->
            filter_map1(
                begin
                    _pipe@1 = Acc,
                    gleam@dict:insert(_pipe@1, Tree, P)
                end,
                Map,
                P
            );

        _ ->
            Acc
    end.

-file("src/pov.gleam", 79).
-spec filter_map0(
    gleam@dict:dict(ASC, list(ASC)),
    gleam@dict:dict(ASC, ASC),
    ASC
) -> gleam@dict:dict(ASC, list(ASC)).
filter_map0(Acc, Map1, Tree) ->
    case begin
        _pipe = Map1,
        maps:size(_pipe)
    end
    > 0 of
        true ->
            P@1 = case begin
                _pipe@1 = Map1,
                gleam_stdlib:map_get(_pipe@1, Tree)
            end of
                {ok, P} -> P;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"pov"/utf8>>,
                                function => <<"filter_map0"/utf8>>,
                                line => 82,
                                value => _assert_fail,
                                start => 1886,
                                'end' => 1927,
                                pattern_start => 1897,
                                pattern_end => 1902})
            end,
            New_acc = begin
                _pipe@2 = Acc,
                gleam@dict:upsert(_pipe@2, P@1, fun(Li) -> case Li of
                            {some, El} ->
                                _pipe@3 = El,
                                gleam@list:filter(
                                    _pipe@3,
                                    fun(E) -> E /= Tree end
                                );

                            none ->
                                []
                        end end)
            end,
            New_map1 = begin
                _pipe@4 = Map1,
                gleam@dict:delete(_pipe@4, Tree)
            end,
            filter_map0(New_acc, New_map1, P@1);

        _ ->
            Acc
    end.

-file("src/pov.gleam", 98).
-spec make_tree(
    tree(ASJ),
    {gleam@dict:dict(ASJ, list(ASJ)), gleam@dict:dict(ASJ, ASJ)}
) -> tree(ASJ).
make_tree(Acc, Map) ->
    Acc@1 = case begin
        _pipe = erlang:element(1, Map),
        gleam_stdlib:map_get(_pipe, erlang:element(2, Acc))
    end of
        {ok, Children} ->
            {tree,
                erlang:element(2, Acc),
                begin
                    _pipe@1 = Children,
                    gleam@list:fold(_pipe@1, [], fun(Ac, Cu) -> _pipe@2 = Ac,
                            _pipe@4 = gleam@list:prepend(
                                _pipe@2,
                                make_tree(
                                    {tree, Cu, []},
                                    {erlang:element(1, Map),
                                        begin
                                            _pipe@3 = erlang:element(2, Map),
                                            gleam@dict:delete(_pipe@3, Cu)
                                        end}
                                )
                            ),
                            lists:reverse(_pipe@4) end)
                end};

        _ ->
            {tree, erlang:element(2, Acc), []}
    end,
    case begin
        _pipe@5 = erlang:element(2, Map),
        gleam_stdlib:map_get(_pipe@5, erlang:element(2, Acc@1))
    end of
        {ok, Parent_label} ->
            Children@1 = begin
                _pipe@6 = erlang:element(3, Acc@1),
                lists:append(
                    _pipe@6,
                    [make_tree({tree, Parent_label, []}, Map)]
                )
            end,
            {tree, erlang:element(2, Acc@1), Children@1};

        _ ->
            Acc@1
    end.

-file("src/pov.gleam", 11).
-spec from_pov(tree(ARE), ARE) -> {ok, tree(ARE)} | {error, nil}.
from_pov(Tree, From) ->
    Map = make_map({maps:new(), maps:new()}, Tree),
    New_root_tree_el = begin
        _pipe = erlang:element(1, Map),
        gleam_stdlib:map_get(_pipe, From)
    end,
    case New_root_tree_el of
        {ok, _} ->
            Map_1 = filter_map1(maps:new(), erlang:element(2, Map), From),
            Map_0 = filter_map0(erlang:element(1, Map), Map_1, From),
            Res = make_tree({tree, From, []}, {Map_0, Map_1}),
            {ok, Res};

        _ ->
            {error, nil}
    end.

-file("src/pov.gleam", 129).
-spec search(
    list(ASR),
    {gleam@dict:dict(ASR, list(ASR)), gleam@dict:dict(ASR, ASR)},
    ASR,
    ASR
) -> list(ASR).
search(Acc, Map, Tree, Fin) ->
    case Tree =:= Fin of
        true ->
            Acc;

        false ->
            P = begin
                _pipe = erlang:element(2, Map),
                gleam_stdlib:map_get(_pipe, Tree)
            end,
            Pass_parent_acc = case P of
                {ok, Pa} ->
                    case begin
                        _pipe@1 = Acc,
                        gleam@list:contains(_pipe@1, Pa)
                    end of
                        true ->
                            Acc;

                        false ->
                            search(
                                begin
                                    _pipe@2 = Acc,
                                    lists:append(_pipe@2, [Pa])
                                end,
                                Map,
                                Pa,
                                Fin
                            )
                    end;

                _ ->
                    Acc
            end,
            case begin
                _pipe@3 = Pass_parent_acc,
                gleam@list:contains(_pipe@3, Fin)
            end of
                true ->
                    Pass_parent_acc;

                false ->
                    Children = begin
                        _pipe@4 = erlang:element(1, Map),
                        _pipe@5 = gleam_stdlib:map_get(_pipe@4, Tree),
                        gleam@result:unwrap(_pipe@5, [])
                    end,
                    case begin
                        _pipe@6 = Children,
                        gleam@list:filter(
                            _pipe@6,
                            fun(Ch) -> not (gleam@list:contains(Acc, Ch)) end
                        )
                    end of
                        [] ->
                            Pass_parent_acc;

                        Children@1 ->
                            _pipe@7 = Children@1,
                            _pipe@9 = gleam@list:map(
                                _pipe@7,
                                fun(El) ->
                                    search(
                                        begin
                                            _pipe@8 = Acc,
                                            lists:append(_pipe@8, [El])
                                        end,
                                        Map,
                                        El,
                                        Fin
                                    )
                                end
                            ),
                            _pipe@11 = gleam@list:filter(
                                _pipe@9,
                                fun(El@1) -> _pipe@10 = El@1,
                                    gleam@list:contains(_pipe@10, Fin) end
                            ),
                            _pipe@12 = gleam@list:sort(
                                _pipe@11,
                                fun(A, B) ->
                                    gleam@int:compare(
                                        erlang:length(A),
                                        erlang:length(B)
                                    )
                                end
                            ),
                            _pipe@13 = gleam@list:first(_pipe@12),
                            gleam@result:unwrap(_pipe@13, [])
                    end
            end
    end.

-file("src/pov.gleam", 29).
-spec path_to(tree(ARJ), ARJ, ARJ) -> {ok, list(ARJ)} | {error, nil}.
path_to(Tree, From, To) ->
    Map = make_map({maps:new(), maps:new()}, Tree),
    case {begin
            _pipe = erlang:element(1, Map),
            gleam_stdlib:map_get(_pipe, From)
        end,
        begin
            _pipe@1 = erlang:element(2, Map),
            gleam_stdlib:map_get(_pipe@1, From)
        end,
        begin
            _pipe@2 = erlang:element(1, Map),
            gleam_stdlib:map_get(_pipe@2, To)
        end,
        begin
            _pipe@3 = erlang:element(2, Map),
            gleam_stdlib:map_get(_pipe@3, To)
        end} of
        {{error, _}, {error, _}, _, _} ->
            {error, nil};

        {_, _, {error, _}, {error, _}} ->
            {error, nil};

        {_, _, _, _} ->
            Res = search([From], Map, From, To),
            {ok, Res}
    end.

-file("src/pov.gleam", 182).
-spec main() -> {ok, list(binary())} | {error, nil}.
main() ->
    path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"a"/utf8>>, []},
                {tree, <<"x"/utf8>>, []},
                {tree, <<"b"/utf8>>, []},
                {tree, <<"c"/utf8>>, []}]},
        <<"a"/utf8>>,
        <<"c"/utf8>>
    ).
