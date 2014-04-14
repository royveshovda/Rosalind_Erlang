-module(sign).
-export([generate/1, test/0]).


test() ->
	generate_singed_from_list([1,2], []).
	%Perms = generate(2),
	%perm:print_perms(Perms).

generate(N) ->
	%TODO: Create variations with signs
	Perms = perm:generate(N),
	generate_singed_from_array(Perms, []).


generate_singed_from_array([], Acc) ->
	Acc;
generate_singed_from_array([Head|Tail], Acc) ->
	ok.


generate_singed_from_list([], Acc) ->
	Acc;
generate_singed_from_list([Head|Tail], Acc) ->
	Tail_as_signed = generate_singed_from_list(Tail, Acc),
	Acc1 = [Head|Tail_as_signed],
	Acc2 = [-Head|Tail_as_signed],
	Acc1 ++ Acc2.