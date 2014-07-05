-module(splc).
-export([test/0, test2/0]).

test() ->

	%Dna_string = "ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG",
	Dna_string = "ATGGTCTACATAGCTGACAAACAGCACGTAGCATCTCGAGAGGCATATGGTCACATGTTCAAAGTTTGCGCCTAG",
	Dna = convert:string_to_dna(Dna_string),
	Intron_strings = 	[
							"ATCGGTCGAA",
							"ATCGGTCGAGCGTGT"
						],
	Introns = convert:list_of_strings_to_dna(Intron_strings),
	New_Dna = remove_introns(Dna, Introns),
	Rna = rna:transcribe(New_Dna),
	Protein = prot:rna_to_protein(Rna),
	basic:print_protein(Protein).



test2() ->
	Dna_string = "ATGGTCTACAGTCTACA",
	Intron_string = "GTCTA",
	%Expect: ATGCACA
	Dna = convert:string_to_dna(Dna_string),
	Intron = convert:string_to_dna(Intron_string),
	remove_intron(Dna, Intron).


remove_introns(Dna, []) ->
	Dna;
remove_introns(Dna, [Intron | Rest_of_introns]) ->
	New_Dna = remove_intron(Dna, Intron),
	remove_introns(New_Dna, Rest_of_introns).


remove_intron(Dna, Intron) ->
	Motifs = subs:motif(Dna, Intron),
	remove_sub_strings(Dna, Motifs, length(Intron)).

remove_sub_strings(String, Starting_positions, Length) ->
	Ordered_starting_positions = lists:reverse(lists:sort(Starting_positions)),
	remove_sub_strings2(String, Ordered_starting_positions, Length).

remove_sub_strings2(String, [], _Length) ->
	String;
remove_sub_strings2(String, [Last_starting_position | Rest_of_starting_positions], Length) ->
	New_string = remove_sub_string(String, Last_starting_position, Length),
	remove_sub_strings2(New_string, Rest_of_starting_positions, Length).

remove_sub_string(String, Starting_position, Length) ->
	Part1 = lists:sublist(String, 1, Starting_position),
	Part2 = lists:sublist(String, Starting_position+Length),
	Part1++Part2.



%MVYIADKQHVASREAYGHMFKVCA


%ATGGTCTACATAGCTGACAAACAGCACGTAGCATCTCGAGAGGCATATGGTCACATGTTCAAAGTTTGCGCCTAG

%>Rosalind_10
%ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
%>Rosalind_12
%ATCGGTCGAA
%>Rosalind_15
%ATCGGTCGAGCGTGT