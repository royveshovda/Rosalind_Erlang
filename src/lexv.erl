-module(lexv).
-export([generate/2, test/0]).

test() ->
	%TODO: Implement

	%Result = generate(["T","A","G","C"], 2),
	Result = generate(["D","Z","P","K", "W", "M"], 3),
	print(Result).

generate(Lex, Length) ->
	generate(Lex,Length, Lex).

generate(_Lex, 1, Acc) ->
	Acc;
generate(Lex, Length, Acc) ->
	New_Acc = [[Acc1|Lex1] || Acc1 <- Acc, Lex1 <- Lex],
	generate(Lex, Length -1, New_Acc).

print([]) ->
	ok;
print([Head|Tail]) ->
	io:format("~s~n", [Head]),
	print(Tail).