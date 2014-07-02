-module(lexv).
-export([generate/2, test/0]).

test() ->
	Result = generate(["D","N","A"], 3),
	Result.
	%print(Result).

get_pad() ->
	"Ø".

generate(Lex, Length) ->
	generate(Lex, Length, get_pad()).

generate(Lex, Length, Pad) ->
	%Add empty-padding to all except the first instance
	Generated_raw = generate1([Pad|Lex],Length, Lex),
	remove_trailing_empties(Generated_raw, Pad).

generate1(_Lex, 1, Acc) ->
	Acc;
generate1(Lex, Length, Acc) ->
	New_Acc = [lists:flatten([Acc1|Lex1]) || Acc1 <- Acc, Lex1 <- Lex],
	generate1(Lex, Length -1, New_Acc).


remove_trailing_empties(Lex, Pad) ->
	remove_trailing_empties(Lex, Pad, []).

remove_trailing_empties([], _Pad, Acc) ->
	lists:reverse(Acc);
remove_trailing_empties([Head|Tail], Pad, Acc) ->
	Trimmed = remove_trailing_empties_from_list(Head, Pad),
	remove_trailing_empties(Tail, Pad, [Trimmed|Acc]).


remove_trailing_empties_from_list(List, Pad) ->
	Reverse = lists:reverse(List),
	remove_trailing_empties_from_list1(Reverse, Pad).

remove_trailing_empties_from_list1([Pad|Rest], Pad) ->
	remove_trailing_empties_from_list1(Rest, Pad);
remove_trailing_empties_from_list1([Head|Tail], _Pad) ->
	lists:reverse([Head|Tail]).

%remove Ø in the end
%remove words with Ø in the middle


% print([]) ->
% 	ok;
% print([Head|Tail]) ->
% 	io:format("~s~n", [Head]),
% 	print(Tail).