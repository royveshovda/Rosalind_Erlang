-module(perm).
-export([generate/1, test/0]).

test() ->
	generate(3).

generate(N) ->
	BaseList = get_list_of_all_numbers(N),
	Perm = generate(BaseList, []).

generate([],Perm) ->
	Perm;
generate(List, Perm) ->
	Length = length(List).

generate(List, Position, Length, Perm) ->
	ok.

get_list_of_all_numbers(N) ->
	get_list_of_all_numbers(N, []).

get_list_of_all_numbers(0,List) ->
	List;
get_list_of_all_numbers(N, List) ->
	get_list_of_all_numbers(N-1, [N | List]).