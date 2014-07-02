-module(revc).
-export([complementing/1, complementing_test/0]).

complementing_test() ->
	TestInput = convert:string_to_dna("AAAACCCGGT"),
	Expected = convert:string_to_dna("ACCGGGTTTT"),
	Actual = complementing(TestInput),
	{complementary, Expected} = Actual,
	ok.

complementing(Dna) ->
	complementing(Dna, []).

complementing([], Comp) ->
	{complementary, Comp};
complementing([dna_a|RestOfDna], Complementary) ->
	complementing(RestOfDna, [dna_t|Complementary]);
complementing([dna_c|RestOfDna], Complementary) ->
	complementing(RestOfDna, [dna_g|Complementary]);
complementing([dna_g|RestOfDna], Complementary) ->
	complementing(RestOfDna, [dna_c|Complementary]);
complementing([dna_t|RestOfDna], Complementary) ->
	complementing(RestOfDna, [dna_a|Complementary]).