-module(sign).
-export([generate/1, test/0]).


test() ->
	Perms = generate(2),
	perm:print_perms(Perms).

generate(N) ->
	%TODO: Create variations with signs
	perm:generate(N).