-module(lcsm).
-export([test/0]).

test() ->
	find_all_substrings([1,2,3,4]).

find_all_substrings(List) ->
	find_all_substrings(List, []).

find_all_substrings([], Acc) ->
	lists:reverse(Acc);
find_all_substrings(List, Acc) ->
	
	[_H|T] = List,
	Subs = find_all_substrings(T),

	NewAcc = [List|Acc] ++ Subs,

	[_ReverseH|ReverseT] = lists:reverse(List),
	Head = lists:reverse(ReverseT),
	find_all_substrings(Head, NewAcc).

% combinations([]) ->
% 	[];
% combinations([H|[]]) ->
% 	[H];
% combinations([H|T]) ->
% 	[]
% 	[X || X <- remove_dups(T), X /= H]


% expand the first string, and get all possible substrings

% use all the possible substrings, and go through them one by one.
% search each og the other strings for motif (subs:motif)
% save candidates along the way

% compare all the candidates, and return the longest