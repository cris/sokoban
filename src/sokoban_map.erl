-module(sokoban_map).
-compile(export_all).

%% coords: starts from 1x1

player(up, Map) ->
  PlayerP = position(player, Map),
  case go(up, PlayerP) of
    fail -> Map;
    PlayerP2={_,_} ->
      moveto(PlayerP, PlayerP2, Map)
  end.


%% private part

% items
player() -> [$*, $+].
box()    -> [$o, $@].
wall()   -> $#.
hole()   -> [$~, $+, $@].
empty()  -> $\ .

moveto(Player, NewPos, Map) ->
  Map2 = remove_from(player, Player, Map),
  place_to(player, NewPos, Map2).

place_to(player, NewPos, Map) ->
    ok.


remove_from(player, Pos={X,Y}, Map) ->
  Item = what_left(Pos, Map).

position(player, Map) ->
    Symbols = player(),
    sokoban_matrix:find_object(Symbols, Map).

go(up, {X,Y}) ->
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

