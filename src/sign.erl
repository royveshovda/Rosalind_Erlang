-module(sign).
-export([generate/1, test/0]).


test() ->
	Perms = generate(3),
	Flat_Perms = flatten_perms(Perms, []),
	perm:print_perms(Flat_Perms).

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
	%TODO: Improvethis, so we do not have to call lists_flatten
	[[H,T] || H <- [Head, -Head], T <- Tail_as_signed ].

flatten_perms([], Acc) ->
	Acc;
flatten_perms([Head|Tail],Acc) ->
	Flat = lists:flatten(Head),
	flatten_perms(Tail, [Flat|Acc]).