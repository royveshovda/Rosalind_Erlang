-module(rna_tests).
-include_lib("eunit/include/eunit.hrl").

transcribe_test() ->
	DnaString = "GATGGAACTTGACTACGTAAATT",
	Dna = basic:string_to_dna(DnaString),
	Rna = rna:transcribe(Dna),
	RnaString = basic:rna_to_string(Rna),
	RnaTestString = "GAUGGAACUUGACUACGUAAAUU",
	RnaString = RnaTestString,
	ok.