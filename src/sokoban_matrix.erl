-module(sokoban_matrix).
-compile(export_all).

set(Char, {X,Y}, Map) when is_integer(Char) ->
    Row = lists:nth(Y, Map),
    lists:nth(X, Row).

get(Map, {X,Y}) ->
  Row = lists:nth(Y, Map),
  lists:nth(X, Row).

find_object(Symbol, Map) ->
  F = fun(L) -> string:chr(L, Symbol) end,
  Pozs = lists:map(F, Map),
  find_iv_by(fun(X) -> X > 0 end, Pozs).


find_iv_by(Pred, L) when is_function(Pred,1), is_list(L) ->
  F = fun(X, {Ax, Ay}) ->
      Nx = case Pred(X) of
        true  -> X;
        false -> Ax
      end,
      if
        Ax > 0 -> {Nx, Ay};
        true   -> {Nx, Ay+1}
      end
  end,
  lists:foldl(F, {0,0}, L).


map_i(F, L) when is_function(F,2), is_list(L) ->
    map_i(F, L, 1, []).

map_i(_F, [], _I, Acc) ->
    lists:reverse(Acc);
map_i(F, [H|T], I, Acc) ->
    map_i(F, T, I+1, [F(H,I) | Acc]).

map_ij(F, L) when is_function(F,3), is_list(L) ->
    Fy = fun(Lx, Y) -> map_i(fun(V, X) -> F(V,X,Y) end, Lx) end,
    map_i(Fy, L).


find_i(F, L) when is_function(F,2), is_list(L) ->
    find_i(F, L, 1).

find_i(_F, [], _I) ->
    none;
find_i(F, [H|T], I) ->
    case F(H, I) of
        true  -> {H, I};
        false -> find_i(F, T, I+1)
    end.

find_ij(F, L) when is_function(F,3), is_list(L) ->
    find_ij(F, L, 1).

find_ij(_F, [], _J) ->
    none;
find_ij(F, [R|T], J) ->
    Fx = fun(V,I) -> F(V,I,J) end,
    case find_i(Fx,R) of
        {V,I}  -> {V,I,J};
        none -> find_ij(F, T, J+1)
    end.
