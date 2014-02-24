-module(sokoban_map).
-compile(export_all).

%% coords: starts from 1x1

player(up, Map) ->
  Player = object(player, Map),
  case go(up, Player, Map) of
    fail -> Map;
    Map2 when is_list(Map2) -> Map2
  end.


%% private part

% items
player() -> [$*, $+].
box()    -> [$o, $@].
wall()   -> $#.
hole()   -> [$~, $+, $@].
empty()  -> $\ .

% Player position is 1, next cell is 2, and next cell is 3
% Depending on what is on cell2 and cell3 we can know how
% to transform the whole stuff
%
% Cell1(Player1) -> Cell2(?) -> Cell3(?)

go(up, Player={P,X,Y}, Map) ->
    Pos2 = position(up, Player),
    Pos3 = position(up, Pos2)
    Item2 = object_at(Pos2, Map),
    Item3 = object_at(Pos3, Map),
    speculate(P, Item2, Item3).

transform_c1_c2_c3(P, Item2, Item3) ->
    TransformC2C3 = speculate(Item2, Item3),
    case TransformC2C3 of
        none -> none;
        [C2] -> [cell1(P), C2];
        [C2,C3] -> [cell1(P), C2, C3]
    end.

cell1({player,Left}) ->
    Left.

% player can't step on wall
speculate(wall, _) ->
    none;
% player can go on empty
speculate(empty, _) ->
    [{player,empty}];
% player can go onto hole
speculate(hole, _) ->
    [{player,hole}];
% box can't move to wall
speculate({box,_}, wall) ->
    none;
% box can move on any base
speculate({box,BaseC2}, BaseC3) ->
    [{player,BaseC2}, {box,BaseC3}];
% two boxes can't move
speculate({box,_}, {box,_}) ->
    none.

% {player, empty} | {player, hole}
% {box, empty} | {box, hole}
% Base: empty | hole
% wall

position(Direction, {_C,X,Y}) when is_atom(Direction) ->
    position(Direction, {X,Y});
position(up, {X,Y}) ->
    {X,Y-1}.

object(player, Map) ->
    Symbols = player(),
    sokoban_matrix:find_object(Symbols, Map).

object_at({X,Y}, Map) ->
    %
    .

