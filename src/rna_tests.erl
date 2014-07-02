-module(rna_tests).
-include_lib("eunit/include/eunit.hrl").

transcribe_test() ->
	DnaString = "GATGGAACTTGACTACGTAAATT",
	Dna = convert:string_to_dna(DnaString),
	Rna = rna:transcribe(Dna),
	RnaString = convert:rna_to_string(Rna),
	RnaTestString = "GAUGGAACUUGACUACGUAAAUU",
	RnaString = RnaTestString,
	ok.