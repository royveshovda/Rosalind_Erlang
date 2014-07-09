-module(lgis).
-export([test/0]).


test() ->
	Permutation1 = [1],
	[1] = get_longest_increasing_subsequence(Permutation1),

	Permutation2 = [1,3,4,2],
	[1,3,4] = get_longest_increasing_subsequence(Permutation2),
	perfect.
%	Decr = get_longest_decreasing_subsequence(Permutaion)

get_longest_increasing_subsequence(List) ->
	List.


%1,3,4
%1,3
%1,4
%1,2
%
%3,4
