-module(lgis).
-export([test/0, run/0]).

run()->
	PermAsString = "5 1 4 2 3",
	Perm = string_to_list(PermAsString),
	{Incr, Decr} = get_longest_subsequences(Perm),
	io:format("~n~nIncreasing: ~n"),
	print_list(Incr),
	io:format("~n~nDecreasing:~n"),
	print_list(Decr).

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

	Perm6 = [1,2,3,4,9,5,6],
	[1,2,3,4,5,6] = get_longest_increasing_subsequence(Perm6),

	Perm7 = [5,1,4,2,3],
	[1,2,3] = get_longest_increasing_subsequence(Perm7),
	[5,4,2] = get_longest_decreasing_subsequence(Perm7),

	Perm8 = [8,2,1,6,5,7,4,3,9],
	[2,6,7,9] = get_longest_increasing_subsequence(Perm8),
	[8,6,5,4,3] = get_longest_decreasing_subsequence(Perm8),

	Perm9 = [2,8,9,4,5,3,6,7,1],
	[2,4,5,6,7] = get_longest_increasing_subsequence(Perm9),

	String1 = "5 1 4 2 3",
	[5,1,4,2,3] = string_to_list(String1),

	String2 = "81 22 13 64 55 76 47 38 99",
	[81,22,13,64,55,76,47,38,99] = string_to_list(String2),

	perfect.

get_longest_subsequences(List) ->
	Incr = get_longest_increasing_subsequence(List),
	Decr = get_longest_decreasing_subsequence(List),
	{Incr, Decr}.

get_longest_increasing_subsequence(List) ->
	Comparer = fun(X,Y) -> X > Y end,
	get_longest(List, Comparer).

get_longest_decreasing_subsequence(List) ->
	Comparer = fun(X,Y) -> X < Y end,
	get_longest(List, Comparer).

string_to_list(String) ->
	lists:map(fun(X) -> {Int, _} = string:to_integer(X), 
                    Int end, 
          string:tokens(String, " ")).

print_list([]) ->
	io:format("~n");
print_list([Head|Tail]) ->
	io:format("~p ", [Head]),
	print_list(Tail).

get_longest(List, Comparer) ->
	lists:reverse(get_longest(List, Comparer, [])).

get_longest([], _Comparer, Longest) ->
	Longest;
get_longest([Head|Tail], Comparer, []) ->
	Candidate1 = get_longest(Tail, Comparer, []),
	Candidate2 = get_longest(Tail, Comparer, [Head]),
	longest(Candidate1, Candidate2);
get_longest([Head|Tail], Comparer, [LHead| _LTail] = Longest) ->
	Candidate1 = get_longest(Tail, Comparer, Longest),
	NewLongest1 = longest(Longest, Candidate1),

	case Comparer(Head, LHead) of
		true ->
			Candidate2 = get_longest(Tail, Comparer, [Head|Longest]),
			longest(NewLongest1, Candidate2);
		false ->
			NewLongest1
	end.

longest(Candidate1, _Candidate2) when length(Candidate1) > length(_Candidate2) ->
	Candidate1;
longest(_Candidate1, Candidate2) when length(_Candidate1) =< length(Candidate2) ->
	Candidate2.
