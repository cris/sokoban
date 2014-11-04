# sokoban - bare game logic engine in Erlang

I wonder - how it would be to write a game logic with bare Erlang. From one side it
can be tricky, because of immutability. Changing the state means copying it along the way
while in any game one can change rapidly.

It turns out, that Erlang pattern-matching facility can greatly simplify a game
logic stuff. Sometimes a code is way better and more clear than in Ruby.

Sokoban was quite a pleasure to write. Its most essential part is boiled down to
the following:

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

As a plan of action, you need to know about the state of two cells going right
next after a character. Getting all combinations can be fully described with simple
pattern-matching rules.

For me, it's a resembled description of rules in Prolog(which isn't a surprise).

Take a look at `test/sokoban_game_tests.erl` to get an idea, how to run this app.
