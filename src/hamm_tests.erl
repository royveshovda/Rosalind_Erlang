-module(hamm_tests).
-include_lib("eunit/include/eunit.hrl").

hamming_distance_test() ->
	Dna1 = convert:string_to_dna("GAGCCTACTAACGGGAT"),
	Dna2 = convert:string_to_dna("CATCGTAATGACGGCCT"),
	Actual = hamm:hamming_distance(Dna1, Dna2),
	Expected = 7,
	Actual = Expected,
	ok.
