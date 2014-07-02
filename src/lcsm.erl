-module(lcsm).
-export([test/0, test1/0, test2/0, test3/0, test4/0, test5/0, process_batch/4]).

%Problems regarding perfomance in this module
%must be improved to complete in time

test3() ->
	List = [1,2,3,4,5,6],
	Match1 = [3,4],
	Match2 = [3,5],
	true = contains(List, Match1),
	false = contains(List, Match2),
	perfect.

test1() ->
	MinLength = 500,
	Filename = "./testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),	

	Strings = get_dna_lists(Dnas),

	SortAsc = fun(A, B) -> length(A) =< length(B) end,
    Sorted = lists:sort(SortAsc, Strings),
    % Reverse the motifs so that we are searching for longest ones first.
    Motifs = lists:reverse(all_substrings(lists:nth(1, Sorted), MinLength)),
    io:format("Found ~p motifs~n", [length(Motifs)]),
	ok.

test2() ->
	Filename = "./testdata/rosalind_lcsm.txt",
	Dnas = basic:open_fasta_file(Filename),	

	Lists = get_dna_lists(Dnas),
	Strings = dna_list_to_string_list(Lists, []),
	find_shared_motif_strings(Strings, 30).

dna_list_to_string_list([], Strings) ->
	Strings;
dna_list_to_string_list([Dna|Rest], Strings) ->
	dna_list_to_string_list(Rest, [convert:dna_to_string(Dna)|Strings]).


test() ->
	B = [1,2,3,4,5],
	generate_combinations(B,1).

test5() ->
	B = [[1,2,3,4,5]],
	generate_batches(B).

test4() ->
	Filename = "./testdata/rosalind_lcsm.txt",
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
	Potentials = generate_combinations(Dna,100),	
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

% Finding a Shared Motif
% http://rosalind.info/problems/lcs/
% Find one longest common substring of all given strings.
% MinLength gives a performance improvement.

find_shared_motif_strings(Strings, MinLength) ->
    % Sort ascending so that we select motifs from the shortest string.
    SortAsc = fun(A, B) -> length(A) =< length(B) end,
    Sorted = lists:sort(SortAsc, Strings),
    % Reverse the motifs so that we are searching for longest ones first.
    Motifs = lists:reverse(all_substrings(lists:nth(1, Sorted), MinLength)),
    io:format("Found ~p motifs~n", [length(Motifs)]),
    Remainder = lists:nthtail(1, Sorted),
    find_in_all(Motifs, Remainder).


% Return a subset of strings in Motifs that exist in all Strings.
find_in_all([], _) ->
    not_found;
find_in_all([Motif|T], Strings) ->
    case is_motif_in_strings(Motif, Strings) of
        true ->
            Motif;
        _ ->
            find_in_all(T, Strings)
    end.

is_motif_in_strings([], []) ->
    false;
is_motif_in_strings(_Motif, []) ->
    true;
is_motif_in_strings(Motif, [S|Tail]) ->
    case find_motif(Motif, S) of
        [] ->
            false;
        L when is_list(L), length(L) > 0 ->
            is_motif_in_strings(Motif, Tail)
    end.

all_substrings(S, MinLength) ->
    all_substrings(S, length(S), MinLength, []).

all_substrings(_S, 0, _MinLength, Acc) ->
    Acc;
all_substrings(_S, Length, MinLength, Acc) when Length < MinLength ->
    Acc;
all_substrings(S, Length, MinLength, Acc) when length(S) =:= Length ->
    all_substrings(S, Length-1, MinLength, [S|Acc]);
all_substrings(S, Length, MinLength, Acc) ->
    Acc2 = find_all_substrings(S, Length, lists:seq(1, length(S) - Length + 1), Acc),
    all_substrings(S, Length-1, MinLength, Acc2).

find_all_substrings(_S, _Length, [], Acc) ->
    Acc;
find_all_substrings(S, Length, [Position|Tail], Acc) ->
    Sub = lists:sublist(S, Position, Length),
    find_all_substrings(S, Length, Tail, [Sub|Acc]).


% Finding a Motif in DNA
% http://rosalind.info/problems/subs/
% Find positions of T in S (1-indexed).
find_motif(T, S) when is_list(T), is_list(S) ->
    find_motif(list_to_binary(T), list_to_binary(S));
find_motif(T, S) when is_binary(T), is_binary(S), size(T) =< size(S) ->
    find_motif_at(T, S, 1, []).

% Call do_find_motif for every value in S.
find_motif_at(_, <<>>, _, Acc) ->
    lists:reverse(Acc);
find_motif_at(T, S, Pos, Acc) ->
    Acc2 = case do_find_motif(T, S) of
        true ->
            [Pos|Acc];
        _ ->
            Acc
    end,
    <<_:8, Rest/binary>> = S,
    find_motif_at(T, Rest, Pos+1, Acc2).

do_find_motif(<<>>, _) ->
    true;
do_find_motif(<<T:8, TRest/binary>>, <<S:8, SRest/binary>>) when T =:= S ->
    do_find_motif(TRest, SRest);
do_find_motif(_, _) ->
    false.

