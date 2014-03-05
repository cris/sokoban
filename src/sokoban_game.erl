-module(sokoban_game).
-compile(export_all).

new(Name) when is_atom(Name) ->
    {state, map(Name)};
new(Map) when is_list(Map) ->
    {state, Map}.

map(simple) ->
  [
    "#####",   % # -> wall
    "#   #",   % ~ -> hole
    "# * #",   %' '-> empty
    "#   #",   % *, + -> player, player on the hole
    "#####"    % o, @ -> box, box on the hole
  ].

action(Way, {state, Map})
        when Way =:= up; Way =:= down; Way =:= left; Way =:= right ->
    Map2 = sokoban_map:player(Way, Map),
    {state, Map2}.
