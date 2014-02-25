-module(sokoban_map).
-compile(export_all).

%% coords: starts from 1x1

player(up, Map) ->
    Cell1 = cell(player, Map),
    Pos1 = position(Cell1),
    Pos2 = position(up, Player),
    Pos3 = position(up, Pos2)
    Cell2 = cell(Pos2, Map),
    Cell3 = cell(Pos3, Map),
    _Map2 = transform_c1_c2_c3(Cell1, Cell2, Cell3, Map).


%% private part

% items
player() -> [$*, $+].
box()    -> [$o, $@].
wall()   -> $#.
hole()   -> [$~, $+, $@].
empty()  -> $\ .

chr2cell($*)  -> {player, empty};
chr2cell($+)  -> {player, hole};
chr2cell($o)  -> {box, empty};
chr2cell($@)  -> {box, hole};
chr2cell($#)  -> wall;
chr2cell($~)  -> hole;
chr2cell($\ ) -> empty.

% Player position is 1, next cell is 2, and next cell is 3
% Depending on what is on cell2 and cell3 we can know how
% to transform the whole stuff
%
% Cell1(Item1={Player1,Base},X1,Y1) -> Cell2(?,X2,Y2) -> Cell3(?,X3,Y3)

transform_c1_c2_c3(Cells, Map) ->
    Items = lists:map(fun item/1, Cells),
    NewItems = transformation(Items),
    NewCells = lists:zipwith(fun newcell/2, NewItems, Cells),
    lists:foldl(fun sokoban_matrix:, Map, NewCells).

transformation([Item1, Item2, Item3]) ->
    TransformC2C3 = speculate(Item2, Item3),
    case TransformC2C3 of
        []      -> [];
        [C2]    -> [speculate(Item1), C2];
        [C2,C3] -> [speculate(Item1), C2, C3]
    end.

speculate({player,Left}) -> Left.

% player can't step on wall
speculate(wall, _)          -> [];
% player can go on empty
speculate(empty, _)         -> [{player,empty}];
% player can go onto hole
speculate(hole, _)          -> [{player,hole}];
% box can't move to wall
speculate({box,_}, wall)    -> [];
% box can move on any base
speculate({box,B2}, B3)     -> [{player,B2}, {box,B3}];
% two boxes can't move
speculate({box,_}, {box,_}) -> [].

% {player, empty} | {player, hole}
% {box, empty} | {box, hole}
% Base: empty | hole
% wall

position({_,X,Y}) ->
    {X,Y}.

position(Direction, {_C,X,Y}) when is_atom(Direction) ->
    position(Direction, {X,Y});
position(up, {X,Y}) ->
    {X,Y-1}.

newcell(NewItem, {_,X,Y}) ->
    {NewItem,X,Y}.

cell(player, Map) ->
    Symbols = player(),
    MaybeCxy = sokoban_matrix:find_object(Symbols, Map),
    cell(MaybeCxy);
cell(XY={_,_}, Map) ->
    MaybeCxy = sokoban_matrix:find_object(XY, Map),
    cell(MaybeCxy).

cell({C,X,Y}) ->
    {chr2cell(C), X, Y};
cell(none) ->
    none.

item({Item,_,_}) ->
    Item.
