-module(lgis).
-export([test/0, get_longest_increasing_subsequence/1]).


test() ->
	Permutation1 = [1],
	[1] = get_longest_increasing_subsequence(Permutation1),

	Permutation2 = [1,3,4,2],
	[1,3,4] = get_longest_increasing_subsequence(Permutation2),
	perfect.
%	Decr = get_longest_decreasing_subsequence(Permutaion)

get_longest_increasing_subsequence(List) ->
	get_longest_increasing_subsequence(List, 0, []).

get_longest_increasing_subsequence([], _LatestAddedToSequence, Result) ->
	lists:reverse(Result);
get_longest_increasing_subsequence([Head|Tail], LatestAddedToSequence, Result) when Head > LatestAddedToSequence ->
	get_longest_increasing_subsequence(Tail, Head, [Head, Result]);
get_longest_increasing_subsequence([_Head|Tail], LatestAddedToSequence, Result) when _Head =< LatestAddedToSequence ->
	get_longest_increasing_subsequence(Tail, LatestAddedToSequence, Result).


%1,3,4
%1,3
%1,4
%1,2
%
%3,4
