-module(perm).
-export([generate/1, print_perms/1, test/0]).

test() ->
	Perms = generate(5),
	print_perms(Perms).

generate(N) ->
	BaseList = lists:seq(1,N),
	perms(BaseList).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

print_perms(Perms) ->
	print_perms_count(Perms),
	print_perms_array(Perms).

print_perms_count(Perms) ->
	Length = length(Perms),
	io:format("~p~n", [Length]).

print_perms_array([]) ->
	void;
print_perms_array([Head|Rest]) ->
	print_perms_list(Head),
	io:format("~n", []),
	print_perms_array(Rest).

print_perms_list([]) ->
	void;
print_perms_list([Head|Rest]) ->
	io:format("~p ", [Head]),
	print_perms_list(Rest).