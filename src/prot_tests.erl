-module(prot_tests).
-include_lib("eunit/include/eunit.hrl").

rna_to_protein_test() ->
	RnaString = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA",
	Rna = convert:string_to_rna(RnaString),
	%"MAMAPRTEINSTRING" = prot:rna_to_protein(Rna),
	Actual = prot:rna_to_protein(Rna),
	Expected = convert:string_to_protein("MAMAPRTEINSTRING."),
	Actual = Expected,
	ok.