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

set_nth(N, F, L) when is_integer(N), is_list(L) ->
    List2 = set_nth(N, F, L, []),
    lists:reverse(List2).

set_nth(N, F, [], Acc) ->
    Acc;
set_nth(1, F, [H|T], Acc) ->
    Value = if
        is_function(F) -> F(H);
        true -> F
    end,
    set_nth(0, F, T, [Value | Acc]);
set_nth(0, F, [H|T], Acc) ->
    set_nth(0, F, T, [H | Acc]);
set_nth(N, F, [H|T], Acc) when N > 1 ->
    set_nth(N-1, F, T, [H | Acc]).


map_i(F, L) when is_function(F,2), is_list(L) ->
    map_i(F, L, 1, []).

map_i(F, [], I, Acc) ->
    lists:reverse(Acc);
map_i(F, [H|T], I, Acc) ->
    map_i(F, T, I+1, [F(H,I) | Acc]).

map_ij(F, L) when is_function(F,3), is_list(L) ->
    Fy = fun(Lx, Y) -> map_i(fun(V, X) -> F(V,X,Y) end, Lx) end,
    map_i(Fy, L).
