-module(lgis).
-export([test/0, get_longest_increasing_subsequence/1]).


test() ->
	Perm1 = [1],
	[1] = get_longest_increasing_subsequence(Perm1),

	Perm2 = [1,2,3],
	[1,2,3] = get_longest_increasing_subsequence(Perm2),
	perfect,

	Perm3 = [1,2,3,2],
	[1,2,3] = get_longest_increasing_subsequence(Perm3),

	Perm4 = [1,2,1,3,4],
	[1,2,3,4] = get_longest_increasing_subsequence(Perm4),

	Perm5 = [1,4,2,3],
	[1,2,3] = get_longest_increasing_subsequence(Perm5),

	perfect.

get_longest_increasing_subsequence(List) ->
	get_longest_increasing_subsequence(List, []).

get_longest_increasing_subsequence([], Result) ->
	lists:reverse(Result);
get_longest_increasing_subsequence([Head|Tail], []) ->
	get_longest_increasing_subsequence(Tail,[Head]);

get_longest_increasing_subsequence([Head1,Head2|Tail],[RHead|_RTail] = Result) when Head2 > Head1, Head1 > RHead ->
	get_longest_increasing_subsequence([Head2|Tail], [Head1|Result]);

get_longest_increasing_subsequence([Head1,Head2|Tail],[RHead|_RTail] = Result) when Head2 =< Head1, Head1 > RHead ->
	Cand1 = get_longest_increasing_subsequence(Tail, [Head1|Result]),
	Cand2 = get_longest_increasing_subsequence(Tail, [Head2|Result]),
	
	L1 = length(Cand1),
	L2 = length(Cand2),

	case L1 >= L2 of
		true ->
			Cand1;
		false ->
			Cand2
	end;

	get_longest_increasing_subsequence([_Head|Tail],[_RHead|_RTail] = Result) when _Head =< _RHead ->
	get_longest_increasing_subsequence(Tail, Result).
%1,3,4
%1,3
%1,4
%1,2
%
%3,4
