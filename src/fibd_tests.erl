-module(fibd_tests).
-include_lib("eunit/include/eunit.hrl").

fib1_test() ->
	Actual = fibd:wabbits(6,3),
	Expected = 4,
	Actual = Expected,
	ok.

% fib2_test() ->
% 	Actual = fib:wabbits(29,3),
% 	Expected = 8878212019,
% 	Actual = Expected,
% 	ok.	