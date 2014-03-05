-module(sokoban_game_tests).
-include_lib("eunit/include/eunit.hrl").

-define(game, sokoban_game).

new_test() ->
    State = sokoban_game:new(simple),
    ?assertMatch({state, [[_|_],[_|_],[_|_],[_|_],[_|_]]}, State).

map(center) ->
    [
        "#####",
        "#   #",
        "# * #",
        "#   #",
        "#####"
    ];
map(up) ->
    [
        "#####",
        "# * #",
        "#   #",
        "#   #",
        "#####"
    ];
map(down) ->
    [
        "#####",
        "#   #",
        "#   #",
        "# * #",
        "#####"
    ];
map(left) ->
    [
        "#####",
        "#   #",
        "#*  #",
        "#   #",
        "#####"
    ];
map(right) ->
    [
        "#####",
        "#   #",
        "#  *#",
        "#   #",
        "#####"
    ].

map(box, center) ->
    [
        "#######",
        "#     #",
        "#  o  #",
        "# o*o #",
        "#  o  #",
        "#     #",
        "#######"
    ];
map(box, down) ->
    [
        "#######",
        "#     #",
        "#  o  #",
        "# o o #",
        "#  *  #",
        "#  o  #",
        "#######"
    ];
map(box, {down,up}) ->
    [
        "#######",
        "#     #",
        "#  o  #",
        "# o*o #",
        "#     #",
        "#  o  #",
        "#######"
    ];
map(box, up) ->
    lists:reverse(map(box, down));
map(box, {up,down}) ->
    lists:reverse(map(box, {down,up}));
map(box, left) ->
    [
        "#######",
        "#     #",
        "#  o  #",
        "#o* o #",
        "#  o  #",
        "#     #",
        "#######"
    ];
map(box, {left,right}) ->
    [
        "#######",
        "#     #",
        "#  o  #",
        "#o *o #",
        "#  o  #",
        "#     #",
        "#######"
    ];
map(box, right) ->
    lists:map(fun lists:reverse/1, map(box, left));
map(box, {right,left}) ->
    lists:map(fun lists:reverse/1, map(box, {left,right})).

opposite(up)    -> down;
opposite(down)  -> up;
opposite(left)  -> right;
opposite(right) -> left.

action_test_direction(Direction, Map1, Map2) ->
    Opposite = opposite(Direction),
    State = sokoban_game:new(Map1),
    State2 = sokoban_game:action(Direction, State),
    ?assertMatch({state, Map2}, State2),
    State3 = sokoban_game:action(Direction, State2),
    ?assertMatch(State2, State3),
    State4 = sokoban_game:action(Opposite, State3),
    ?assertMatch(State, State4).

action_test_box_movement(Direction, Map1, Map2, Map3) ->
    Opposite = opposite(Direction),
    State = sokoban_game:new(Map1),
    State2 = sokoban_game:action(Direction, State),
    ?assertMatch({state, Map2}, State2),
    State3 = sokoban_game:action(Direction, State2),
    ?assertMatch(State2, State3),
    State4 = sokoban_game:action(Opposite, State3),
    ?assertMatch({state, Map3}, State4).

action_up_test() ->
    action_test_direction(up, map(center), map(up)).

action_down_test() ->
    action_test_direction(down, map(center), map(down)).

action_left_test() ->
    action_test_direction(left, map(center), map(left)).

action_box_move_up_test() ->
    action_test_box_movement(up, map(box,center), map(box,up), map(box,{up,down})).

action_box_move_down_test() ->
    action_test_box_movement(down, map(box,center), map(box,down), map(box,{down,up})).

action_box_move_left_test() ->
    action_test_box_movement(left, map(box,center), map(box,left), map(box,{left,right})).

action_box_move_right_test() ->
    action_test_box_movement(right, map(box,center), map(box,right), map(box,{right,left})).
