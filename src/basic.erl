-module(basic).
-export([string_to_dna/1, dna_to_string/1, print_dna/1, open_fasta_file/1, string_to_rna/1, rna_to_string/1, string_to_protein/1, protein_to_string/1]).

string_to_dna(String) ->
	string_to_dna(String, []).

string_to_dna([], ConvertedList) ->
	lists:reverse(ConvertedList);
string_to_dna([$A | RestOfDnaString], ConvertedList) ->
	string_to_dna(RestOfDnaString, [dna_a|ConvertedList]);
string_to_dna([$C | RestOfDnaString], ConvertedList) ->
	string_to_dna(RestOfDnaString, [dna_c|ConvertedList]);
string_to_dna([$G | RestOfDnaString], ConvertedList) ->
	string_to_dna(RestOfDnaString, [dna_g|ConvertedList]);
string_to_dna([$T | RestOfDnaString], ConvertedList) ->
	string_to_dna(RestOfDnaString, [dna_t|ConvertedList]).

string_to_rna(String) ->
	string_to_rna(String, []).

string_to_rna([], ConvertedList) ->
	lists:reverse(ConvertedList);
string_to_rna([$A | RestOfDnaString], ConvertedList) ->
	string_to_rna(RestOfDnaString, [rna_a|ConvertedList]);
string_to_rna([$C | RestOfDnaString], ConvertedList) ->
	string_to_rna(RestOfDnaString, [rna_c|ConvertedList]);
string_to_rna([$G | RestOfDnaString], ConvertedList) ->
	string_to_rna(RestOfDnaString, [rna_g|ConvertedList]);
string_to_rna([$U | RestOfDnaString], ConvertedList) ->
	string_to_rna(RestOfDnaString, [rna_u|ConvertedList]).

remove_line_breaks(String) ->
	remove_line_breaks(String, []).

remove_line_breaks([], RemovedString) ->
	lists:reverse(RemovedString);
remove_line_breaks([$\r|Rest], RemovedString) ->
	remove_line_breaks(Rest, RemovedString);
remove_line_breaks([$\n|Rest], RemovedString) ->
	remove_line_breaks(Rest, RemovedString);
remove_line_breaks([Character|Rest], RemovedString) ->
	remove_line_breaks(Rest, [Character|RemovedString]).

open_fasta_file(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	RawParts = binary:split(Binary, <<$>>>, [global]),
	SplittedParts= split_id_from_content(RawParts, []),
	SplittedParts.

split_id_from_content([],SplittedList) ->
	lists:reverse(SplittedList);
split_id_from_content([<<>>|Rest], SplittedList) ->
	split_id_from_content(Rest, SplittedList);
split_id_from_content([Binary|Rest], SplittedList) ->
	[BinaryId, BinaryContent] = binary:split(Binary, <<$\n>>),
	Raw_Id = binary:bin_to_list(BinaryId),
	Id = remove_line_breaks(Raw_Id),
	%TODO: Try to improve this to not use convertion to string. Pure binary will use less memory, and probably be faster
	StringContent = binary:bin_to_list(BinaryContent),
	TrimmedString = remove_line_breaks(StringContent),
	Content = string_to_dna(TrimmedString),
	split_id_from_content(Rest, [{Id, Content}|SplittedList]).


dna_to_string(Dna) ->
	dna_to_string(Dna, []).
dna_to_string([], String) ->
	lists:reverse(String);
dna_to_string([dna_a | RestOfDnaString], ConvertedList) ->
	dna_to_string(RestOfDnaString, [$A|ConvertedList]);
dna_to_string([dna_c | RestOfDnaString], ConvertedList) ->
	dna_to_string(RestOfDnaString, [$C|ConvertedList]);
dna_to_string([dna_g | RestOfDnaString], ConvertedList) ->
	dna_to_string(RestOfDnaString, [$G|ConvertedList]);
dna_to_string([dna_t | RestOfDnaString], ConvertedList) ->
	dna_to_string(RestOfDnaString, [$T|ConvertedList]).

rna_to_string(Rna) ->
	rna_to_string(Rna, []).
rna_to_string([], String) ->
	lists:reverse(String);
rna_to_string([rna_a | Rest], ConvertedList) ->
	rna_to_string(Rest, [$A|ConvertedList]);
rna_to_string([rna_c | Rest], ConvertedList) ->
	rna_to_string(Rest, [$C|ConvertedList]);
rna_to_string([rna_g | Rest], ConvertedList) ->
	rna_to_string(Rest, [$G|ConvertedList]);
rna_to_string([rna_u | Rest], ConvertedList) ->
	rna_to_string(Rest, [$U|ConvertedList]).

print_dna(Dna) ->
	String = dna_to_string(Dna),
	print_dna_string(String).

print_dna_string(DnaString) ->
	io:format("~s~n",[DnaString]).

string_to_protein(ProteinString) ->
	string_to_protein(ProteinString, []).

string_to_protein([], Protein) ->
	lists:reverse(Protein);
string_to_protein([$A | Rest], Protein) ->
	string_to_protein(Rest, [protein_a|Protein]);
string_to_protein([$C | Rest], Protein) ->
	string_to_protein(Rest, [protein_c|Protein]);
string_to_protein([$D | Rest], Protein) ->
	string_to_protein(Rest, [protein_d|Protein]);
string_to_protein([$E | Rest], Protein) ->
	string_to_protein(Rest, [protein_e|Protein]);
string_to_protein([$F | Rest], Protein) ->
	string_to_protein(Rest, [protein_f|Protein]);
string_to_protein([$G | Rest], Protein) ->
	string_to_protein(Rest, [protein_g|Protein]);
string_to_protein([$H | Rest], Protein) ->
	string_to_protein(Rest, [protein_h|Protein]);
string_to_protein([$I | Rest], Protein) ->
	string_to_protein(Rest, [protein_i|Protein]);
string_to_protein([$K | Rest], Protein) ->
	string_to_protein(Rest, [protein_k|Protein]);
string_to_protein([$L | Rest], Protein) ->
	string_to_protein(Rest, [protein_l|Protein]);
string_to_protein([$M | Rest], Protein) ->
	string_to_protein(Rest, [protein_m|Protein]);
string_to_protein([$N | Rest], Protein) ->
	string_to_protein(Rest, [protein_n|Protein]);
string_to_protein([$P | Rest], Protein) ->
	string_to_protein(Rest, [protein_p|Protein]);
string_to_protein([$Q | Rest], Protein) ->
	string_to_protein(Rest, [protein_q|Protein]);
string_to_protein([$R | Rest], Protein) ->
	string_to_protein(Rest, [protein_r|Protein]);
string_to_protein([$S | Rest], Protein) ->
	string_to_protein(Rest, [protein_s|Protein]);
string_to_protein([$T | Rest], Protein) ->
	string_to_protein(Rest, [protein_t|Protein]);
string_to_protein([$V | Rest], Protein) ->
	string_to_protein(Rest, [protein_v|Protein]);
string_to_protein([$W | Rest], Protein) ->
	string_to_protein(Rest, [protein_w|Protein]);
string_to_protein([$Y | Rest], Protein) ->
	string_to_protein(Rest, [protein_y|Protein]);
string_to_protein([$. | Rest], Protein) ->
	string_to_protein(Rest, [protein_stop|Protein]).

protein_to_string(Protein) ->
	protein_to_string(Protein, []).

protein_to_string([], String) ->
	lists:reverse(String);
protein_to_string([protein_a | Rest], Protein) ->
	protein_to_string(Rest, [$A|Protein]);
protein_to_string([protein_c | Rest], Protein) ->
	protein_to_string(Rest, [$C|Protein]);
protein_to_string([protein_d | Rest], Protein) ->
	protein_to_string(Rest, [$D|Protein]);
protein_to_string([protein_e | Rest], Protein) ->
	protein_to_string(Rest, [$E|Protein]);
protein_to_string([protein_f | Rest], Protein) ->
	protein_to_string(Rest, [$F|Protein]);
protein_to_string([protein_g | Rest], Protein) ->
	protein_to_string(Rest, [$G|Protein]);
protein_to_string([protein_h | Rest], Protein) ->
	protein_to_string(Rest, [$H|Protein]);
protein_to_string([protein_i | Rest], Protein) ->
	protein_to_string(Rest, [$I|Protein]);
protein_to_string([protein_k | Rest], Protein) ->
	protein_to_string(Rest, [$K|Protein]);
protein_to_string([protein_l | Rest], Protein) ->
	protein_to_string(Rest, [$L|Protein]);
protein_to_string([protein_m | Rest], Protein) ->
	protein_to_string(Rest, [$M|Protein]);
protein_to_string([protein_n | Rest], Protein) ->
	protein_to_string(Rest, [$N|Protein]);
protein_to_string([protein_p | Rest], Protein) ->
	protein_to_string(Rest, [$P|Protein]);
protein_to_string([protein_q | Rest], Protein) ->
	protein_to_string(Rest, [$Q|Protein]);
protein_to_string([protein_r | Rest], Protein) ->
	protein_to_string(Rest, [$R|Protein]);
protein_to_string([protein_s | Rest], Protein) ->
	protein_to_string(Rest, [$S|Protein]);
protein_to_string([protein_t | Rest], Protein) ->
	protein_to_string(Rest, [$T|Protein]);
protein_to_string([protein_v | Rest], Protein) ->
	protein_to_string(Rest, [$V|Protein]);
protein_to_string([protein_w | Rest], Protein) ->
	protein_to_string(Rest, [$W|Protein]);
protein_to_string([protein_y | Rest], Protein) ->
	protein_to_string(Rest, [$Y|Protein]);
protein_to_string([protein_stop | Rest], Protein) ->
	protein_to_string(Rest, [$.|Protein]).