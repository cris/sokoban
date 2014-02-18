-module(sokoban_map).
-compile(export_all).

player(up, Map) ->
  Player = player(Map),
  case position(up, Player) of
    fail -> Map;
    Pos2={X,Y} ->
      moveto(Player, Pos2)
  end.

moveto(Player, NewPos) ->


position(up, {X,Y}) ->
  if
    Y-1 >= 0 -> {X, Y-1}
    true     -> fail
  end.

player({state, Map}) ->
  Sym = player(),
  []



item({X,Y}) ->

player() -> $*.
