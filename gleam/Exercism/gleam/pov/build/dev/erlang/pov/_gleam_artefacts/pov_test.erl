-module(pov_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "test/pov_test.gleam").
-export([main/0, reroot_a_tree_so_that_its_root_is_the_specified_node_results_in_the_same_tree_if_the_input_tree_is_a_singleton_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_singleton_tree_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_large_tree_test/0, given_two_nodes_find_the_path_between_them_can_find_path_to_parent_test/0, given_two_nodes_find_the_path_between_them_can_find_path_to_sibling_test/0, given_two_nodes_find_the_path_between_them_can_find_path_to_cousin_test/0, given_two_nodes_find_the_path_between_them_can_find_path_not_involving_root_test/0, given_two_nodes_find_the_path_between_them_can_find_path_from_nodes_other_than_x_test/0, given_two_nodes_find_the_path_between_them_errors_if_destination_does_not_exist_test/0, given_two_nodes_find_the_path_between_them_errors_if_source_does_not_exist_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_one_sibling_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_many_siblings_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_new_root_deeply_nested_in_tree_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_moves_children_of_the_new_root_to_same_level_as_former_parent_test/0, reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_complex_tree_with_cousins_test/0]).

-file("test/pov_test.gleam", 8).
-spec main() -> any().
main() ->
    exercism@test_runner:main().

-file("test/pov_test.gleam", 12).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_results_in_the_same_tree_if_the_input_tree_is_a_singleton_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_results_in_the_same_tree_if_the_input_tree_is_a_singleton_test(
    
) ->
    _pipe = pov:from_pov({tree, <<"x"/utf8>>, []}, <<"x"/utf8>>),
    exercism@should:equal(_pipe, {ok, {tree, <<"x"/utf8>>, []}}).

-file("test/pov_test.gleam", 136).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_singleton_tree_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_singleton_tree_test(
    
) ->
    _pipe = pov:from_pov({tree, <<"x"/utf8>>, []}, <<"nonexistent"/utf8>>),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/pov_test.gleam", 141).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_large_tree_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_errors_if_target_does_not_exist_in_a_large_tree_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"parent"/utf8>>,
            [{tree,
                    <<"x"/utf8>>,
                    [{tree, <<"kid-0"/utf8>>, []}, {tree, <<"kid-1"/utf8>>, []}]},
                {tree, <<"sibling-0"/utf8>>, []},
                {tree, <<"sibling-1"/utf8>>, []}]},
        <<"nonexistent"/utf8>>
    ),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/pov_test.gleam", 153).
-spec given_two_nodes_find_the_path_between_them_can_find_path_to_parent_test() -> {ok,
        list(binary())} |
    {error, nil}.
given_two_nodes_find_the_path_between_them_can_find_path_to_parent_test() ->
    _pipe = pov:path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"x"/utf8>>, []}, {tree, <<"sibling"/utf8>>, []}]},
        <<"x"/utf8>>,
        <<"parent"/utf8>>
    ),
    exercism@should:equal(_pipe, {ok, [<<"x"/utf8>>, <<"parent"/utf8>>]}).

-file("test/pov_test.gleam", 162).
-spec given_two_nodes_find_the_path_between_them_can_find_path_to_sibling_test() -> {ok,
        list(binary())} |
    {error, nil}.
given_two_nodes_find_the_path_between_them_can_find_path_to_sibling_test() ->
    _pipe = pov:path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"a"/utf8>>, []},
                {tree, <<"x"/utf8>>, []},
                {tree, <<"b"/utf8>>, []},
                {tree, <<"c"/utf8>>, []}]},
        <<"x"/utf8>>,
        <<"b"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok, [<<"x"/utf8>>, <<"parent"/utf8>>, <<"b"/utf8>>]}
    ).

-file("test/pov_test.gleam", 176).
-spec given_two_nodes_find_the_path_between_them_can_find_path_to_cousin_test() -> {ok,
        list(binary())} |
    {error, nil}.
given_two_nodes_find_the_path_between_them_can_find_path_to_cousin_test() ->
    _pipe = pov:path_to(
        {tree,
            <<"grandparent"/utf8>>,
            [{tree,
                    <<"parent"/utf8>>,
                    [{tree,
                            <<"x"/utf8>>,
                            [{tree, <<"kid-0"/utf8>>, []},
                                {tree, <<"kid-1"/utf8>>, []}]},
                        {tree, <<"sibling-0"/utf8>>, []},
                        {tree, <<"sibling-1"/utf8>>, []}]},
                {tree,
                    <<"uncle"/utf8>>,
                    [{tree, <<"cousin-0"/utf8>>, []},
                        {tree, <<"cousin-1"/utf8>>, []}]}]},
        <<"x"/utf8>>,
        <<"cousin-1"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok,
            [<<"x"/utf8>>,
                <<"parent"/utf8>>,
                <<"grandparent"/utf8>>,
                <<"uncle"/utf8>>,
                <<"cousin-1"/utf8>>]}
    ).

-file("test/pov_test.gleam", 195).
-spec given_two_nodes_find_the_path_between_them_can_find_path_not_involving_root_test(
    
) -> {ok, list(binary())} | {error, nil}.
given_two_nodes_find_the_path_between_them_can_find_path_not_involving_root_test(
    
) ->
    _pipe = pov:path_to(
        {tree,
            <<"grandparent"/utf8>>,
            [{tree,
                    <<"parent"/utf8>>,
                    [{tree, <<"x"/utf8>>, []},
                        {tree, <<"sibling-0"/utf8>>, []},
                        {tree, <<"sibling-1"/utf8>>, []}]}]},
        <<"x"/utf8>>,
        <<"sibling-1"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok, [<<"x"/utf8>>, <<"parent"/utf8>>, <<"sibling-1"/utf8>>]}
    ).

-file("test/pov_test.gleam", 210).
-spec given_two_nodes_find_the_path_between_them_can_find_path_from_nodes_other_than_x_test(
    
) -> {ok, list(binary())} | {error, nil}.
given_two_nodes_find_the_path_between_them_can_find_path_from_nodes_other_than_x_test(
    
) ->
    _pipe = pov:path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"a"/utf8>>, []},
                {tree, <<"x"/utf8>>, []},
                {tree, <<"b"/utf8>>, []},
                {tree, <<"c"/utf8>>, []}]},
        <<"a"/utf8>>,
        <<"c"/utf8>>
    ),
    exercism@should:equal(
        _pipe,
        {ok, [<<"a"/utf8>>, <<"parent"/utf8>>, <<"c"/utf8>>]}
    ).

-file("test/pov_test.gleam", 224).
-spec given_two_nodes_find_the_path_between_them_errors_if_destination_does_not_exist_test(
    
) -> {ok, list(binary())} | {error, nil}.
given_two_nodes_find_the_path_between_them_errors_if_destination_does_not_exist_test(
    
) ->
    _pipe = pov:path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree,
                    <<"x"/utf8>>,
                    [{tree, <<"kid-0"/utf8>>, []}, {tree, <<"kid-1"/utf8>>, []}]},
                {tree, <<"sibling-0"/utf8>>, []},
                {tree, <<"sibling-1"/utf8>>, []}]},
        <<"x"/utf8>>,
        <<"nonexistent"/utf8>>
    ),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/pov_test.gleam", 237).
-spec given_two_nodes_find_the_path_between_them_errors_if_source_does_not_exist_test(
    
) -> {ok, list(binary())} | {error, nil}.
given_two_nodes_find_the_path_between_them_errors_if_source_does_not_exist_test(
    
) ->
    _pipe = pov:path_to(
        {tree,
            <<"parent"/utf8>>,
            [{tree,
                    <<"x"/utf8>>,
                    [{tree, <<"kid-0"/utf8>>, []}, {tree, <<"kid-1"/utf8>>, []}]},
                {tree, <<"sibling-0"/utf8>>, []},
                {tree, <<"sibling-1"/utf8>>, []}]},
        <<"nonexistent"/utf8>>,
        <<"x"/utf8>>
    ),
    exercism@should:equal(_pipe, {error, nil}).

-file("test/pov_test.gleam", 250).
-spec normalize(pov:tree(binary())) -> pov:tree(binary()).
normalize(Tree) ->
    {tree, Label, Children} = Tree,
    Children@1 = begin
        _pipe = Children,
        _pipe@1 = gleam@list:map(_pipe, fun normalize/1),
        gleam@list:sort(
            _pipe@1,
            fun(A, B) ->
                gleam@string:compare(erlang:element(2, A), erlang:element(2, B))
            end
        )
    end,
    {tree, Label, Children@1}.

-file("test/pov_test.gleam", 17).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_one_sibling_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_one_sibling_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"x"/utf8>>, []}, {tree, <<"sibling"/utf8>>, []}]},
        <<"x"/utf8>>
    ),
    _pipe@1 = gleam@result:map(_pipe, fun normalize/1),
    exercism@should:equal(
        _pipe@1,
        {ok,
            {tree,
                <<"x"/utf8>>,
                [{tree, <<"parent"/utf8>>, [{tree, <<"sibling"/utf8>>, []}]}]}}
    ).

-file("test/pov_test.gleam", 32).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_many_siblings_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_a_parent_and_many_siblings_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"parent"/utf8>>,
            [{tree, <<"a"/utf8>>, []},
                {tree, <<"b"/utf8>>, []},
                {tree, <<"c"/utf8>>, []},
                {tree, <<"x"/utf8>>, []}]},
        <<"x"/utf8>>
    ),
    _pipe@1 = gleam@result:map(_pipe, fun normalize/1),
    exercism@should:equal(
        _pipe@1,
        {ok,
            {tree,
                <<"x"/utf8>>,
                [{tree,
                        <<"parent"/utf8>>,
                        [{tree, <<"a"/utf8>>, []},
                            {tree, <<"b"/utf8>>, []},
                            {tree, <<"c"/utf8>>, []}]}]}}
    ).

-file("test/pov_test.gleam", 56).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_new_root_deeply_nested_in_tree_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_tree_with_new_root_deeply_nested_in_tree_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"level-0"/utf8>>,
            [{tree,
                    <<"level-1"/utf8>>,
                    [{tree,
                            <<"level-2"/utf8>>,
                            [{tree,
                                    <<"level-3"/utf8>>,
                                    [{tree, <<"x"/utf8>>, []}]}]}]}]},
        <<"x"/utf8>>
    ),
    _pipe@1 = gleam@result:map(_pipe, fun normalize/1),
    exercism@should:equal(
        _pipe@1,
        {ok,
            {tree,
                <<"x"/utf8>>,
                [{tree,
                        <<"level-3"/utf8>>,
                        [{tree,
                                <<"level-2"/utf8>>,
                                [{tree,
                                        <<"level-1"/utf8>>,
                                        [{tree, <<"level-0"/utf8>>, []}]}]}]}]}}
    ).

-file("test/pov_test.gleam", 81).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_moves_children_of_the_new_root_to_same_level_as_former_parent_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_moves_children_of_the_new_root_to_same_level_as_former_parent_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"parent"/utf8>>,
            [{tree,
                    <<"x"/utf8>>,
                    [{tree, <<"kid-0"/utf8>>, []}, {tree, <<"kid-1"/utf8>>, []}]}]},
        <<"x"/utf8>>
    ),
    _pipe@1 = gleam@result:map(_pipe, fun normalize/1),
    exercism@should:equal(
        _pipe@1,
        {ok,
            {tree,
                <<"x"/utf8>>,
                [{tree, <<"kid-0"/utf8>>, []},
                    {tree, <<"kid-1"/utf8>>, []},
                    {tree, <<"parent"/utf8>>, []}]}}
    ).

-file("test/pov_test.gleam", 100).
-spec reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_complex_tree_with_cousins_test(
    
) -> {ok, pov:tree(binary())} | {error, nil}.
reroot_a_tree_so_that_its_root_is_the_specified_node_can_reroot_a_complex_tree_with_cousins_test(
    
) ->
    _pipe = pov:from_pov(
        {tree,
            <<"grandparent"/utf8>>,
            [{tree,
                    <<"parent"/utf8>>,
                    [{tree,
                            <<"x"/utf8>>,
                            [{tree, <<"kid-0"/utf8>>, []},
                                {tree, <<"kid-1"/utf8>>, []}]},
                        {tree, <<"sibling-0"/utf8>>, []},
                        {tree, <<"sibling-1"/utf8>>, []}]},
                {tree,
                    <<"uncle"/utf8>>,
                    [{tree, <<"cousin-0"/utf8>>, []},
                        {tree, <<"cousin-1"/utf8>>, []}]}]},
        <<"x"/utf8>>
    ),
    _pipe@1 = gleam@result:map(_pipe, fun normalize/1),
    exercism@should:equal(
        _pipe@1,
        {ok,
            {tree,
                <<"x"/utf8>>,
                [{tree, <<"kid-0"/utf8>>, []},
                    {tree, <<"kid-1"/utf8>>, []},
                    {tree,
                        <<"parent"/utf8>>,
                        [{tree,
                                <<"grandparent"/utf8>>,
                                [{tree,
                                        <<"uncle"/utf8>>,
                                        [{tree, <<"cousin-0"/utf8>>, []},
                                            {tree, <<"cousin-1"/utf8>>, []}]}]},
                            {tree, <<"sibling-0"/utf8>>, []},
                            {tree, <<"sibling-1"/utf8>>, []}]}]}}
    ).
