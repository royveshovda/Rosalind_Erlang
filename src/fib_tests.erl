-module(fib_tests).
-include_lib("eunit/include/eunit.hrl").

fib1_test() ->
	Actual = fib:wabbits(5,3),
	Expected = 19,
	Actual = Expected,
	ok.

fib2_test() ->
	Actual = fib:wabbits(29,3),
	Expected = 8878212019,
	Actual = Expected,
	ok.	