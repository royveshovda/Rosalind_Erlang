-module(lcsm).
-export([test/0, test3/0, test4/0, test5/0, process_batch/4]).

test3() ->
	List = [1,2,3,4,5,6],
	Match1 = [3,4],
	Match2 = [3,5],
	true = contains(List, Match1),
	false = contains(List, Match2),
	perfect.

test() ->
	B = [1,2,3,4,5],
	generate_combinations(B).

test5() ->
	B = [[1,2,3,4,5]],
	generate_batches(B).

test4() ->
	Filename = "../testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),	

	Lists = get_dna_lists(Dnas),
	[Dna1|RestOfDnas] = Lists,

	Batches = generate_batches(Dna1),

	create_workers(lists:reverse(Batches), RestOfDnas, 0),

	process_responses(length(Batches)).

create_workers([], _RestOfDnas, _SerialNumber) ->
	ok;
create_workers([Batch1|Rest], RestOfDnas, SerialNumber) ->
	spawn(?MODULE, process_batch, [Batch1, RestOfDnas, self(), SerialNumber]),
	create_workers(Rest, RestOfDnas, SerialNumber+1).


process_responses(NumbersToReceive) ->
	process_responses(NumbersToReceive, 0, []).

process_responses(NumbersToReceive, Received, Longest) when NumbersToReceive == Received ->
	Longest;
process_responses(NumbersToReceive, Received, Longest) ->
	receive
		{response, _SerialNumber, LongestCandidate} ->
			io:format("Received: ~p~n", [Received+1]),
			NewLongest = get_longest_list(Longest, LongestCandidate),
			process_responses(NumbersToReceive, Received+1, NewLongest)
	end.



process_batch(Dna, RestOfDnas, ResponsePid, SerialNumber) ->
	Potentials = generate_combinations(Dna),	
	CommonPotentialsStrings = find_all_common_with_all_dnas(RestOfDnas, Potentials),
	Longest = find_longest_list(CommonPotentialsStrings),
	ResponsePid ! {response, SerialNumber, Longest}.

generate_batches(Dna1) ->
	generate_batches(Dna1, []).


generate_batches([], SubLists) ->
	SubLists;
generate_batches([Head|Tail], SubLists) ->
	generate_batches(Tail, [[Head|Tail]|SubLists]).

get_dna_lists(Dnas) ->
	get_dna_lists(Dnas, []).

get_dna_lists([], Acc) ->
	Acc;
get_dna_lists([{_Id, Dna}|Rest], Acc) ->
	get_dna_lists(Rest, [Dna|Acc]).

get_longest_list(L1, L2) when length(L1) >= length(L2) ->
	L1;
get_longest_list(L1, L2) when length(L1) < length(L2) ->
	L2.

generate_combinations([Head|Tail]) ->
	generate_combinations(Head, Tail, []).

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
	%print_length(length(RestOfTheCandidates)),
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