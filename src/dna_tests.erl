-module(dna_tests).
-include_lib("eunit/include/eunit.hrl").

count_test() ->
	DnaString = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC",
	Dna = basic:string_to_dna(DnaString),
	{counted, 20, 12, 17, 21} = dna:count(Dna),
	ok.