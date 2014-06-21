-module(lcsm).
-export([test/0]).

test() ->
	Filename = "../testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),
	[{_Id1, Dna1}|Rest] = Dnas,
	Potentials = generate_all_substrings(Dna1),
	length(Potentials).
	%Longest = find_longest_common(Rest, Potentials),
	%basic:dna_to_string(Longest).

find_longest_common(Dnas, Potentials) ->
	CommonStrings = find_all_common_with_all_dnas(Dnas, Potentials),
	find_longest_list(CommonStrings).
	%TODO: Find longest in list

find_all_common_with_all_dnas([], Potentials) ->
	lists:reverse(Potentials);
find_all_common_with_all_dnas([{_Id, Dna} | RestOfDna], Potentials) ->
	NewPotentials = find_all_common_with_one_dna(Dna, Potentials),
	find_all_common_with_all_dnas(RestOfDna, NewPotentials).

find_all_common_with_one_dna(Dna, Potentials) ->
	find_all_common_with_one_dna(Dna, Potentials, []).

find_all_common_with_one_dna(_Dna, [], Acc) ->
	Acc;
find_all_common_with_one_dna(Dna, [Candidate|RestOfTheCandidates], Acc) ->
	case subs:motif(Dna, Candidate) of
		[] ->
			find_all_common_with_one_dna(Dna, RestOfTheCandidates, Acc);
		_ ->
			find_all_common_with_one_dna(Dna, RestOfTheCandidates, [Candidate|Acc])
	end.


find_longest_list(Lists) ->
	find_longest_list(Lists, []).

find_longest_list([], Longest) ->
	Longest;
find_longest_list([Candidate | Rest], Longest) when length(Candidate) > length(Longest) ->
	find_longest_list(Rest, Candidate);
find_longest_list([Candidate | Rest], Longest) when length(Candidate) =< length(Longest) ->
	find_longest_list(Rest, Longest).


%TODO: Too slow
generate_all_substrings(List) ->
	generate_all_substrings(List, []).

generate_all_substrings([], Acc) ->
	Acc2 = basic:remove_dups(Acc),
	lists:reverse(Acc2);
generate_all_substrings(List, Acc) ->	
	[_H|T] = List,
	Subs = generate_all_substrings(T),
	NewAcc = [List|Acc] ++ Subs,
	[_ReverseH|ReverseT] = lists:reverse(List),
	Head = lists:reverse(ReverseT),
	generate_all_substrings(Head, NewAcc).

% expand the first string, and get all possible substrings

% use all the possible substrings, and go through them one by one.
% search each og the other strings for motif (subs:motif)
% save candidates along the way

% compare all the candidates, and return the longest