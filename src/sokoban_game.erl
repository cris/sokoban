-module(sokoban_game).
-compile(export_all).

new(Name) ->
  {state, map(Name)}.

map(simple) ->
  [
    "#####",   % # -> wall
    "# * #",   % ~ -> hole
    "#   #",   % *, + -> player, player on the hole
    "#   #",   % o, @ -> box, box on the hole
    "#####"
  ].

action(up, {state, Map}) ->
  Map2 = sokoban_map:player(up, Map),
  {state, Map2}.
