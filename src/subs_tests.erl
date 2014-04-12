-module(subs_tests).
-include_lib("eunit/include/eunit.hrl").

subs_test() ->
	DnaString = "GATATATGCATATACTT",
	MatchString = "ATAT",
	Expected = [2,4,10],
	Actual = subs:motif(DnaString, MatchString),
	Expected = Actual,
	ok.