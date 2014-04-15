-module(sign).
-export([generate/1, flatten_perms/1, test/0]).


test() ->
	Perms = generate(3),
	perm:print_perms(Perms).

generate(N) ->
	Perms = perm:generate(N),
	generate_singed_from_array(Perms, []).

generate_singed_from_array([], Acc) ->
	Acc;
generate_singed_from_array([Head|Tail], Acc) ->
	Perms = generate_singed_from_list(Head),
	generate_singed_from_array(Tail, Acc ++ Perms).

generate_singed_from_list([]) ->
	[];
generate_singed_from_list([Head|[]]) ->
	[Head, -Head];
generate_singed_from_list([Head|Tail]) ->
	Tail_as_signed = generate_singed_from_list(Tail),
	[lists:flatten([H,T]) || H <- [Head, -Head], T <- Tail_as_signed ].