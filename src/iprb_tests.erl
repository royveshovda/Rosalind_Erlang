-module(iprb_tests).
-include_lib("eunit/include/eunit.hrl").

prob_test()->
	Expected = 0.7833333333333333,
	Actual = iprb:prob(2, 2, 2),
	Actual = Expected,
	ok.
