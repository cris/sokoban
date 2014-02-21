-module(sokoban_matrix_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(M, sokoban_matrix).

set_nth_test() ->
    List = [1,2,3,4,5],
    ?assertMatch([9,2,3,4,5], ?M:set_nth(1,9,List)),
    ?assertMatch([1,2,9,4,5], ?M:set_nth(3,9,List)),
    ?assertMatch([1,2,3,4,9], ?M:set_nth(5,9,List)).

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
