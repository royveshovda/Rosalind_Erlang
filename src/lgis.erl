-module(lgis).
-export([test/0, test2/0]).


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
	%[5,4,2] = get_longest_decreasing_subsequence(Perm7),

	Perm8 = [8,2,1,6,5,7,4,3,9],
	[2,6,7,9] = get_longest_increasing_subsequence(Perm8),
	%[8,6,5,4,3] = get_longest_decreasing_subsequence(Perm8),
	

	%BUG: Must be fixed to check all sub-sequences, if they are longer, not only the two first candidates
	Perm9 = [2,8,9,4,5,3,6,7,1],
	[2,4,5,6,7] = get_longest_increasing_subsequence(Perm9),

	

	String1 = "5 1 4 2 3",
	[5,1,4,2,3] = string_to_list(String1),

	String2 = "81 22 13 64 55 76 47 38 99",
	[81,22,13,64,55,76,47,38,99] = string_to_list(String2),

	perfect.

test2()->
	Perm = [2,8,9,4,5,3,6,7,1],
	[2,4,5,6,7] = get_longest_increasing_subsequence(Perm),
	perfect.


get_longest_increasing_subsequence(List) ->
	get_longest_increasing_subsequence(List, []).

get_longest_increasing_subsequence([], Result)->
	Result;
get_longest_increasing_subsequence([_Head|Tail] = List, Result) ->
	Candidate = get_longest_increasing_subsequence2(List),
	case length(Candidate) > length(Result) of
		true ->
			get_longest_increasing_subsequence(Tail, Candidate);
		false ->
			get_longest_increasing_subsequence(Tail, Result)
	end.


get_longest_increasing_subsequence2(List) ->
	get_longest_increasing_subsequence2(List, []).

get_longest_increasing_subsequence2([], Result) ->
	lists:reverse(Result);
get_longest_increasing_subsequence2([Head1,Head2|Tail],[RHead|_RTail] = Result) when Head2 > Head1, Head1 > RHead ->
	get_longest_increasing_subsequence2([Head2|Tail], [Head1|Result]);
get_longest_increasing_subsequence2([Head1,Head2|Tail],[RHead|_RTail] = Result) when Head2 =< Head1, Head1 > RHead ->
	Cand1 = get_longest_increasing_subsequence2(Tail, [Head1|Result]),
	Cand2 = get_longest_increasing_subsequence2(Tail, [Head2|Result]),
	L1 = length(Cand1),
	L2 = length(Cand2),

	case L1 >= L2 of
		true ->
			Cand1;
		false ->
			Cand2
	end;
get_longest_increasing_subsequence2([Head|Tail], []) ->
	get_longest_increasing_subsequence2(Tail,[Head]);
get_longest_increasing_subsequence2([Head|Tail], [RHead|_RTail] = Result) when Head > RHead ->
	get_longest_increasing_subsequence2(Tail, [Head|Result]);
get_longest_increasing_subsequence2([_Head|Tail], [_RHead|_RTail] = Result) when _Head =< _RHead ->
	get_longest_increasing_subsequence2(Tail, Result);
get_longest_increasing_subsequence2([_Head|Tail],[_RHead|_RTail] = Result) when _Head =< _RHead ->
	get_longest_increasing_subsequence2(Tail, Result).


string_to_list(String) ->
	lists:map(fun(X) -> {Int, _} = string:to_integer(X), 
                    Int end, 
          string:tokens(String, " ")).

%print_list([]) ->
%	io:format("~n");
%print_list([Head|Tail]) ->
%	io:format("~p ", [Head]),
%	print_list(Tail).
