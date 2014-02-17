-module(sokoban_game).
-compile(export_all).

new(simple) ->
  [
    "#####",   % # -> wall
    "#o ~#",   % *, + -> player, player on the hole
    "#* @#",   % ~, +, @ -> hole, player on the hole, box on the hole
    "#  *#",   % o -> box
    "#####"
  ].
