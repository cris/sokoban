-module(sokoban_map).
-compile(export_all).

%% coords: starts from 1x1

player(up, Map) ->
    Cell1 = cell(player, Map),
    Cell2 = cell(up, Cell1, Map),
    Cell3 = cell(up, Cell2, Map),
    _Map2 = transform_c1_c2_c3([Cell1, Cell2, Cell3], Map).

%% private part

% items
player() -> [$*, $+].
box()    -> [$o, $@].
wall()   -> $#.
hole()   -> [$~, $+, $@].
empty()  -> $\ .

chr2item($*)  -> {player, empty};
chr2item($+)  -> {player, hole};
chr2item($o)  -> {box, empty};
chr2item($@)  -> {box, hole};
chr2item($#)  -> wall;
chr2item($~)  -> hole;
chr2item($\ ) -> empty.

item2chr({player, empty}) -> $*;
item2chr({player, hole})  -> $+;
item2chr({box, empty})    -> $o;
item2chr({box, hole})     -> $@;
item2chr(wall)            -> $#;
item2chr(hole)            -> $~;
item2chr(empty)           -> $\ .

chrcell({Item,X,Y}) ->
    Chr = item2chr(Item),
    {Chr,X,Y}.

% Player position is 1, next cell is 2, and next cell is 3
% Depending on what is on cell2 and cell3 we can know how
% to transform all 3 cells
%
% Cell1(Item1={Player1,Base},X1,Y1) -> Cell2(?,X2,Y2) -> Cell3(?,X3,Y3)

transform_c1_c2_c3(Cells, Map) ->
    Items = lists:map(fun item/1, Cells),
    NewItems = transformation(Items),
    NewCells = lists:zipwith(fun newcell/2, NewItems, lists:sublist(Cells, length(NewItems))),
    ChrCells = lists:map(fun chrcell/1, NewCells),
    lists:foldl(fun sokoban_matrix:set/2, Map, ChrCells).

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
% two boxes can't move
speculate({box,_}, {box,_}) -> [];
% box can move on any base
speculate({box,B2}, B3) when is_atom(B3) -> [{player,B2}, {box,B3}].

% {player, empty} | {player, hole}
% {box, empty} | {box, hole}
% Base: empty | hole
% wall

position(Direction, {_C,X,Y}) when is_atom(Direction) ->
    position(Direction, {X,Y});
position(up, {X,Y}) ->
    {X, Y-1}.

newcell(NewItem, {_,X,Y}) ->
    {NewItem,X,Y}.

cell(player, Map) ->
    Symbols = player(),
    MaybeCxy = sokoban_matrix:find_object(Symbols, Map),
    cell(MaybeCxy);
cell(XY={_,_}, Map) ->
    MaybeCxy = sokoban_matrix:find_object(XY, Map),
    cell(MaybeCxy).

cell(Direction, Cell, Map) ->
    Pos = position(Direction, Cell),
    cell(Pos, Map).

cell({C,X,Y}) ->
    {chr2item(C), X, Y};
cell(none) ->
    none.

item({Item,_,_}) ->
    Item.
