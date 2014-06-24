-module(lcsm).
-export([test/0, test2/0, test3/0]).

test3() ->
	List = [1,2,3,4,5,6],
	Match1 = [3,4],
	Match2 = [3,5],
	true = contains(List, Match1),
	false = contains(List, Match2),
	perfect.

test2() ->
	%generate_substrings([1,2,3,4]).
	Filename = "../testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),
	[{_Id1, Dna1}|Rest] = Dnas,
	[{_Id2, Dna2}|_Rest] = Rest,

	Potentials = generate_substrings(Dna1),
	io:format("Potentials: ~p~n", [length(Potentials)]),

	Filtered = basic:remove_dups(Potentials),
	io:format("Filtered: ~p~n", [length(Filtered)]).
	%.

	%find_all_common_with_one_dna(Dna2, Potentials).


test() ->
	Filename = "../testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),
	[{_Id1, Dna1}|Rest] = Dnas,
	Potentials = generate_substrings(Dna1),
	%length(Potentials).
	Longest = find_longest_common(Rest, Potentials),
	basic:dna_to_string(Longest).

generate_substrings(List) ->
	generate_substrings(List, []).

generate_substrings([], SubLists) ->
	SubLists;
generate_substrings([Head|Tail], SubLists) ->
	NewSubLists = generate_combinations(Head, Tail, []),
	generate_substrings(Tail, NewSubLists ++ SubLists).

generate_combinations(Head, [], Combinations) ->
	[[Head]|Combinations];
generate_combinations(Head, Tail, Combinations) ->
	NewCombinations = [[Head|Tail]|Combinations],
	NewTail = cut_last_element(Tail),
	generate_combinations(Head, NewTail, NewCombinations).

cut_last_element(List) when length(List) =< 1 ->
	[];
cut_last_element(List) ->
	Length = length(List),
	lists:sublist(List,1,Length-1).


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
	io:format("~p~n", [length(RestOfTheCandidates)]),
	case contains(Dna, Candidate) of
		false ->
			find_all_common_with_one_dna(Dna, RestOfTheCandidates, Acc);
		true ->
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

% expand the first string, and get all possible substrings

% use all the possible substrings, and go through them one by one.
% search each og the other strings for motif (subs:motif)
% save candidates along the way

% compare all the candidates, and return the longest


contains(List, Match) ->
	contains(List, Match, 1).

contains([], _Match, _Position) ->
	false;
contains(Content, Match, Position) ->
	LengthMatch = length(Match),
	LengthContent = length(Content),
	if
		LengthContent - Position < LengthMatch -> false;
		LengthContent - Position >= LengthMatch ->
			SubContent = lists:sublist(Content, Position, LengthMatch),
			case SubContent =:= Match of
				true ->
					true;
				false ->
					contains(Content, Match, Position+1)
			end
	end.