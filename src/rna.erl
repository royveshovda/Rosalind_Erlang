-module(rna).
-export([transcribe/1]).

-spec transcribe(types:dnas()) -> types:rnas().

transcribe(Dna) ->
	transcribe(Dna, []).

transcribe([], Rna) ->
	lists:reverse(Rna);
transcribe([dna_a|Tail], Rna) ->
	transcribe(Tail, [rna_a|Rna]);
transcribe([dna_c|Tail], Rna) ->
	transcribe(Tail, [rna_c|Rna]);
transcribe([dna_g|Tail], Rna) ->
	transcribe(Tail, [rna_g|Rna]);
transcribe([dna_t|Tail], Rna) ->
	transcribe(Tail, [rna_u|Rna]).