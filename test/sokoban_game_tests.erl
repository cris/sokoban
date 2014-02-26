-module(sokoban_game_tests).
-include_lib("eunit/include/eunit.hrl").

-define(game, sokoban_game).

new_test() ->
  State = sokoban_game:new(simple),
  ?assertMatch({state, [[_|_],[_|_],[_|_],[_|_],[_|_]]}, State).

action_up_test() ->
  State = sokoban_game:new(simple),
  State2 = sokoban_game:action(up, State),
  ExpectedMap = [
    "#####",
    "# * #",
    "#   #",
    "#   #",
    "#####"
  ],
  ?assertMatch({state, ExpectedMap}, State2),
  State3 = sokoban_game:action(up, State),
  ExpectedMap = [
    "#####",
    "# * #",
    "#   #",
    "#   #",
    "#####"
  ],
  ?assertMatch({state, ExpectedMap}, State3).
