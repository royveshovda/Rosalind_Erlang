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

	Lists = get_dna_lists(Dnas),
	find_all_sublists(Lists).


get_dna_lists(Dnas) ->
	get_dna_lists(Dnas, []).

get_dna_lists([], Acc) ->
	Acc;
get_dna_lists([{_Id, Dna}|Rest], Acc) ->
	get_dna_lists(Rest, [Dna|Acc]).


find_all_sublists([Dna1 | RestOfDnas]) ->
	Longest = process_batch(Dna1, RestOfDnas, []),

	Longest.
process_batch([], _RestOfDnas, Longest) ->
	Longest;
process_batch([_Head|[]], _RestOfDnas, Longest) ->
	Longest;
process_batch([Head|Tail], RestOfDnas, Longest) ->
	Potentials = generate_combinations(Head, Tail, [], Longest),
	
	CommonPotentialsStrings = find_all_common_with_all_dnas(RestOfDnas, Potentials),
	NewLongestCandidate = find_longest_list(CommonPotentialsStrings),
	NewLongest = get_longest_list(Longest, NewLongestCandidate),
	io:format("~p~n", [NewLongest]),
	process_batch(Tail, RestOfDnas, NewLongest).

get_longest_list(L1, L2) when length(L1) >= length(L2) ->
	L1;
get_longest_list(L1, L2) when length(L1) < length(L2) ->
	L2.

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
	NewSubLists = generate_combinations(Head, Tail, [], []),
	generate_substrings(Tail, NewSubLists ++ SubLists).


generate_combinations(Head, [], Combinations, _Longest) ->
	[[Head]|Combinations];
generate_combinations(Head, Tail, Combinations, Longest) ->
	NewCombinations = [[Head|Tail]|Combinations],
	NewTail = cut_last_element(Tail),
	case length(NewTail) >= length(Longest) of
		true ->
			generate_combinations(Head, NewTail, NewCombinations, Longest);
		false ->
			Combinations
	end.

cut_last_element(List) when length(List) =< 1 ->
	[];
cut_last_element(List) ->
	Length = length(List),
	lists:sublist(List,1,Length-1).


find_longest_common(Dnas, Potentials) ->
	CommonStrings = find_all_common_with_all_dnas(Dnas, Potentials),
	find_longest_list(CommonStrings).

find_all_common_with_all_dnas([], Potentials) ->
	lists:reverse(Potentials);
find_all_common_with_all_dnas([Dna | RestOfDnas], Potentials) ->
	NewPotentials = find_all_common_with_one_dna(Dna, Potentials),
	find_all_common_with_all_dnas(RestOfDnas, NewPotentials).

find_all_common_with_one_dna(Dna, Potentials) ->
	find_all_common_with_one_dna(Dna, Potentials, []).

find_all_common_with_one_dna(_Dna, [], Acc) ->
	Acc;
find_all_common_with_one_dna(Dna, [Candidate|RestOfTheCandidates], Acc) ->
	print_length(length(RestOfTheCandidates)),
	case contains(Dna, Candidate) of
		false ->
			find_all_common_with_one_dna(Dna, RestOfTheCandidates, Acc);
		true ->
			find_all_common_with_one_dna(Dna, RestOfTheCandidates, [Candidate|Acc])
	end.

print_length(Length) when Length rem 100 =:= 0 ->
	io:format("~p~n", [Length]);
print_length(_Length) ->
	void.


find_longest_list(Lists) ->
	find_longest_list(Lists, []).

find_longest_list([], Longest) ->
	Longest;
find_longest_list([Candidate | Rest], Longest) when length(Candidate) > length(Longest) ->
	find_longest_list(Rest, Candidate);
find_longest_list([Candidate | Rest], Longest) when length(Candidate) =< length(Longest) ->
	find_longest_list(Rest, Longest).



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