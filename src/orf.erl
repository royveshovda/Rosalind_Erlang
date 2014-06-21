-module(orf).
-export([find_orfs/1, test/0, test2/0]).

get_frames(Dna) ->	
	Length = length(Dna),
	Length1 = Length - (Length rem 3),
	Length2 = (Length - 1) - ((Length - 1) rem 3),
	Length3 = (Length - 2) - ((Length - 2) rem 3),

	Dna1 = lists:sublist(Dna,1,Length1),
	Dna2 = lists:sublist(Dna,2,Length2),
	Dna3 = lists:sublist(Dna,3,Length3),
	{complementary, Dna_reverse1} = revc:complementing(Dna1),
	{complementary, Dna_reverse2} = revc:complementing(Dna2),
	{complementary, Dna_reverse3} = revc:complementing(Dna3),
	[Dna1, Dna2, Dna3, Dna_reverse1, Dna_reverse2, Dna_reverse3].

test() ->
	L1 = [protein_a, protein_stop, protein_m],
	L2 = [protein_a, protein_e, protein_d, protein_stop, protein_m,protein_stop, protein_m,protein_d,protein_stop,protein_m],
	[[protein_a]] = split_protein_based_on_stop(L1),
	[[protein_a, protein_e, protein_d], [protein_m], [protein_m, protein_d]] = split_protein_based_on_stop(L2),

	L3 = [protein_m],
	L4 = [protein_a, protein_m],
	L5 = [protein_a, protein_m, protein_e, protein_d],
	L6 = [protein_a, protein_m, protein_e, protein_m, protein_d],
	[[protein_m]] = find_substrings_starting_with_protein_m(L3),
	[[protein_m]] = find_substrings_starting_with_protein_m(L4),
	[[protein_m, protein_e, protein_d]] = find_substrings_starting_with_protein_m(L5),
	[[protein_m, protein_e, protein_m, protein_d], [protein_m, protein_d]] = find_substrings_starting_with_protein_m(L6),

	[ [dna_a, dna_g, dna_c, dna_c, dna_t, dna_c]
		, [dna_g, dna_c, dna_c, dna_t, dna_c, dna_g]
		, [dna_c, dna_c, dna_t]
		, [dna_t, dna_c, dna_g, dna_g, dna_a, dna_g]
		, [dna_c, dna_g, dna_g, dna_a, dna_g, dna_c]
		, [dna_g, dna_g, dna_a] ] 
		= get_frames([dna_a, dna_g, dna_c, dna_c, dna_t, dna_c, dna_g]),


	ok.


test2() ->
	InFilename = "../testdata/Rosalind_orf.txt",
	OutFilename = "../testdata/Rosalind_orf_out.txt",

	[{_Id, Dna}] = basic:open_fasta_file(InFilename),
	
	%length(Dna).
	%DnaFrames = get_frames(Dna),

	%lists:map(fun(X) -> 1000+length(X) end, DnaFrames).

	Orfs = find_orfs(Dna),


	file:delete(OutFilename),
	{ok, IoDevice} = file:open(OutFilename, [write]),
	lists:map(fun(X) -> append_protein_to_file(X, IoDevice) end, Orfs),


	ok.

append_protein_to_file(Protein, IoDevice) ->
	ProteinString = basic:protein_to_string(Protein),
	file:write(IoDevice, ProteinString),
	file:write(IoDevice, "\n").



find_orfs(Dna) ->
	DnaFrames = get_frames(Dna),
	Proteins = lists:map(fun(X) -> dna_to_protein:translate(X) end, DnaFrames),
	SplittedProteins = split_proteins(Proteins),
	Orfs = find_orf_proteins(SplittedProteins),
	SingleOrfs = basic:remove_dups(Orfs),
	SingleOrfs.

split_proteins(ListOfProteins) ->
	split_proteins(ListOfProteins, []).

split_proteins([], Acc) ->
	Acc;
split_proteins([Head|Tail], Acc) ->
	Splits = split_protein_based_on_stop(Head),
	NewAcc = Acc ++ Splits,
	split_proteins(Tail, NewAcc).


split_protein_based_on_stop(Protein) ->
	Stops = subs:motif(Protein, [protein_stop]),
	split_protein(Protein, Stops, 1, []).

split_protein([], _, _, Acc)
	-> lists:reverse(Acc);
split_protein(_Protein, [], _, Acc) ->
	lists:reverse(Acc);
split_protein(Protein, [NextStop|Stops], Position, Acc) ->
	Sub = lists:sublist(Protein, Position, NextStop-Position),
	split_protein(Protein, Stops, NextStop+1, [Sub|Acc]).

find_orf_proteins(ListOfProteins) ->
	find_orf_proteins(ListOfProteins, []).

find_orf_proteins([], Acc) ->
	Acc;
find_orf_proteins([Head|Tail], Acc) ->
	Subs = find_substrings_starting_with_protein_m(Head),
	NewAcc = Acc ++ Subs,
	find_orf_proteins(Tail, NewAcc).


find_substrings_starting_with_protein_m(Protein) ->
	find_substrings_starting_with_protein_m(Protein, []).

find_substrings_starting_with_protein_m([],  Acc) ->
	lists:reverse(Acc);
find_substrings_starting_with_protein_m([protein_m|RestOfProtein], Acc) ->
	find_substrings_starting_with_protein_m(RestOfProtein, [[protein_m|RestOfProtein]|Acc]);
find_substrings_starting_with_protein_m([_Other|RestOfProtein], Acc) ->
	find_substrings_starting_with_protein_m(RestOfProtein, Acc).

% find_orfs_protein(Protein) ->
% 	Result = lists:member(protein_stop, Protein).
% 	First = lists:splitwith()
% 	%ok.

% find_orfs_protein([], Splitted_proteins) ->
% 	Splitted_proteins;
% find_orfs_protein(Protein, Splitted_proteins) ->
% 	Result = lists:member(protein_stop, Protein),
% 	case Result of
% 		false ->
% 			Splitted_proteins;
% 		true ->
% 			{L1, L2} = lists:splitwith(fun(X) -> X =/= protein_stop end, Protein),

% 	end.

	