-module(lcsm).
-export([test1/0, test2/0, process_batch/4, find_longest/1]).

test1() ->
	List = [1,2,3,4,5,6],
	Match1 = [3,4],
	Match2 = [3,5],
	true = contains(List, Match1),
	false = contains(List, Match2),
	perfect.

test2() ->
	Filename = "./testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),	
	Lists = get_dna_lists(Dnas),
	Longest = find_longest(Lists),
	io:format("~n~nResult:~n"),
	basic:print_dna(Longest).

find_longest(Lists) ->
	[Dna1|RestOfDnas] = Lists,
	Batches = generate_batches(Dna1),
	create_workers(lists:reverse(Batches), RestOfDnas, 0),
	Longest = process_responses(length(Batches)),
	Longest.


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
			io:format("Received: ~p (~p)~n", [Received+1, length(LongestCandidate)]),
			NewLongest = get_longest_list(Longest, LongestCandidate),
			process_responses(NumbersToReceive, Received+1, NewLongest)
	end.



process_batch(Dna, RestOfDnas, ResponsePid, SerialNumber) ->
	Potentials = generate_combinations(Dna,1),	
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

generate_combinations([Head|Tail], MinLength) ->
	generate_combinations(Head, Tail, [], MinLength).

generate_combinations(Head, [], Combinations, _MinLength) ->
	[[Head]|Combinations];
generate_combinations(Head, Tail, Combinations, MinLength) when (1 + length(Tail)) >= MinLength ->
	NewCombinations = [[Head|Tail]|Combinations],
	NewTail = cut_last_element(Tail),
	generate_combinations(Head, NewTail, NewCombinations, MinLength);
generate_combinations(_Head, _Tail, Combinations, _MinLength) when (1 + length(_Tail)) < _MinLength ->
	Combinations.

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

% print_length(Length) when Length rem 100 =:= 0 ->
% 	io:format("~p~n", [Length]);
% print_length(_Length) ->
% 	void.


find_longest_list(Lists) ->
	find_longest_list(Lists, []).

find_longest_list([], Longest) ->
	Longest;
find_longest_list([Candidate | Rest], Longest) when length(Candidate) > length(Longest) ->
	find_longest_list(Rest, Candidate);
find_longest_list([Candidate | Rest], Longest) when length(Candidate) =< length(Longest) ->
	find_longest_list(Rest, Longest).



contains([],_Match) ->
	false;
contains([Head|Tail], Match) ->
	case starts_with([Head|Tail], Match) of 
		true ->
			true;
		false ->
			contains(Tail,Match)
	end.


starts_with([Pattern|List], [Pattern|Match]) ->
	starts_with(List,Match);
starts_with(_List,[]) ->
	true;
starts_with([],_Match) ->
	false;
starts_with(_List,_Match) ->
	false.
