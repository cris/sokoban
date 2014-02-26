-module(sokoban_matrix_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(M, sokoban_matrix).

map_i_test() ->
    List = [1,2,3,4,5],
    F = fun(V,I) when I =:= 3 -> V + 100;
           (V,_) -> V end,
    ?assertMatch([1,2,103,4,5], ?M:map_i(F, List)).

map_ij_test() ->
    List = ["hello","world"],
    F = fun(_,I,J) when I =:= 2, J =:= 2 -> $0;
           (_,I,J) when I =:= 2, J =:= 1 -> $3;
           (V,_,_) -> V end,
    ?assertMatch(["h3llo","w0rld"], ?M:map_ij(F, List)).

fixture(map) ->
    [
        "########",
        "#      #",
        "#   *  #",
        "#+   @ #",
        "#  o ~ #",
        "########"
    ];
fixture(list) ->
    "abcdef".

find_object_test() ->
    Map = fixture(map),
    ?assertMatch({$*,5,3}, ?M:find_object($*, Map)),
    ?assertMatch({$@,6,4}, ?M:find_object([$o,$@], Map)).

find_i_via_index_test() ->
    List = fixture(list),
    F = fun(_,I) -> I =:= 3 end,
    ?assertMatch({$c, 3}, ?M:find_i(F, List)),
    F2 = fun(_,I) -> I =:= 9 end,
    ?assertMatch(none, ?M:find_i(F2, List)).

find_i_via_value_test() ->
    List = fixture(list),
    F = fun(V,_) -> V =:= $d end,
    ?assertMatch({$d,4}, ?M:find_i(F, List)),
    F2 = fun(V,_) -> V =:= $$ end,
    ?assertMatch(none, ?M:find_i(F2, List)).

find_ij_via_indexes_test() ->
    Map = fixture(map),
    F = fun(_,I,J) -> I =:= 6 andalso J =:= 4 end,
    ?assertMatch({$@,6,4}, ?M:find_ij(F, Map)),
    F2 = fun(_,I,J) -> I =:= 60 andalso J =:= 4 end,
    ?assertMatch(none, ?M:find_ij(F2, Map)).

find_ij_via_value_test() ->
    Map = fixture(map),
    F = fun(V,_,_) -> V =:= $o end,
    ?assertMatch({$o,4,5}, ?M:find_ij(F, Map)),
    F2 = fun(V,_,_) -> V =:= $! end,
    ?assertMatch(none, ?M:find_ij(F2, Map)).

set_test() ->
    Map = fixture(map),
    Map2 = ?M:set({$+,5,3}, Map),
    {Line3, _} = ?M:find_i(fun(_,I) -> I =:= 3 end, Map2),
    ?assertMatch("#   +  #", Line3).
