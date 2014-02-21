-module(sokoban_map).
-compile(export_all).

%% coords: starts from 1x1

player(up, Map) ->
  Player = player(Map),
  case position(up, Player) of
    fail -> Map;
    Pos2={_X,_Y} ->
      moveto(Player, Pos2, Map)
  end.

moveto(Player, NewPos, Map) ->
  Map2 = remove_from(player, Player, Map),
  place_to(player, NewPos, Map2).

place_to(player, NewPos, Map) ->
    ok.


remove_from(player, Pos={X,Y}, Map) ->
  Item = what_left(Pos, Map).

position(up, {X,Y}) ->
  if
    Y-1 > 0 -> {X, Y-1};
    true    -> fail
  end.

player(Map) ->
  Sym = player(),
  sokoban_matrix:find_object(Sym, Map).

what_left({X,Y}, Map) ->
  Item = sokoban_matrix:get({X,Y}, Map),
  case Item of
    $+  -> $~;
    $@  -> $o
  end.

player() -> $*.
