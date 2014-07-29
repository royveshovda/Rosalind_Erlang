-module(basic).
-export([print_dna/1, print_protein/1, open_fasta_file/1, remove_dups/1, remove_line_breaks/1]).

-spec print_dna(types:dnas()) -> ok.
-spec print_protein(types:proteins()) -> ok.
-spec remove_dups(list()) -> list().
-spec open_fasta_file(string()) -> [types:dna_with_id()]. 

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].

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
	Content = convert:string_to_dna(TrimmedString),
	split_id_from_content(Rest, [{Id, Content}|SplittedList]).

print_dna(Dna) ->
	String = convert:dna_to_string(Dna),
	print_dna_string(String).

print_dna_string(DnaString) ->
	io:format("~s~n",[DnaString]).

print_protein(Protein) ->
	String = convert:protein_to_string(Protein),
	print_protein_string(String).

print_protein_string(ProteinString) ->
	io:format("~s~n",[ProteinString]).
