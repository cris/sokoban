# sokoban - bare game logic engine in Erlang

I wonder - how it would be to write game logic in bare Erlang. From one side it
can be tricky, because of immutability. Changing state - is always copying. And
in any game state can be changed very quickly.

It turns out, that Erlang pattern-matching facility can greatly simplify game
logic stuff. Sometimes code is much better and more clear than in Ruby.

Sokoban was quite pleasure to write. It's most essential part is boiled down to
this:

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

To know, what to do, you need to know about state of two next cells after
character. Getting all combinations it can be fully described with simple
pattern-matching rules.

For me, it's resembled description of rules in Prolog(which isn't surprise).

Take a look at `test/sokoban_game_tests.erl` to get an idea, how to run this
app. 
