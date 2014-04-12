-module(revp).
-export([search/1, search_file/1, test/1]).

test(Filename) ->
	Res = search_file(Filename),
	print_findings(Res).

search_file(Filename) ->
	[{_Id, Dna}| _Rest] = basic:open_fasta_file(Filename),
	%Dna = basic:string_to_dna("TGCA"),
	%is_revese_palindrome(Dna).
	%Dna = basic:string_to_dna("TCAATGCATGCGGGTCTATATGCAT"),
	search(Dna).

search(Dna) ->
	search(Dna, 1, []).

search(Dna, Position, Findings) when Position + 4 > (length(Dna)+1) ->
	lists:reverse(Findings);
search(Dna, Position, Findings) when Position + 4 =< (length(Dna)+1) ->
	L4 = lists:sublist(Dna,Position,4),
	case is_revese_palindrome(L4) of
		true ->
			F4 = [{Position, 4} | Findings];
		false ->
			F4 = Findings
	end,

	if
		Position + 6 =< (length(Dna)+1) ->
			L6 = lists:sublist(Dna,Position,6),
			case is_revese_palindrome(L6) of
				true ->
					F6 = [{Position, 6} | F4];
				false ->
					F6 = F4
			end;
		Position + 6 > length(Dna) ->
			F6 = F4
	end,

	if
		Position + 8 =< (length(Dna)+1) ->
			L8 = lists:sublist(Dna,Position,8),
			case is_revese_palindrome(L8) of
				true ->
					F8 = [{Position, 8} | F6];
				false ->
					F8 = F6
			end;
		Position + 8 > length(Dna) ->
			F8 = F6
	end,

	if
		Position + 10 =< (length(Dna)+1) ->
			L10 = lists:sublist(Dna,Position,10),
			case is_revese_palindrome(L10) of
				true ->
					F10 = [{Position, 10} | F8];
				false ->
					F10 = F8
			end;
		Position + 10 > length(Dna) ->
			F10 = F8
	end,

	if
		Position + 12 =< (length(Dna)+1) ->
			L12 = lists:sublist(Dna,Position,12),
			case is_revese_palindrome(L12) of
				true ->
					F12 = [{Position, 12} | F10];
				false ->
					F12 = F10
			end;
		Position + 12 > length(Dna) ->
			F12 = F10
	end,
	search(Dna, Position+1, F12).



print_findings([]) ->
	ok;
print_findings([{Pos, Len} | Rest]) ->
	io:format("~p ~p~n",[Pos, Len]),
	print_findings(Rest).

is_revese_palindrome(Dna) ->
	Dna =:= reverse_complementary(Dna).

reverse_complementary(Dna) ->
	{complementary, Complementary} = revc:complementing(Dna),
	Complementary.

% search(Dna, Position, Findings) ->
% 	ok.



%loop through all positions in whole string.
	% get all substrings at position with length between 4 and 12
		% Check if is reverse palindrome
		% Search for reverse palindrome in the full string
% Return all matches for reverse palindromes


%TCAATGCATGCGGGTCTATATGCAT

% 4 6
% 5 4
% 6 6
% 7 4
% 17 4
% 18 4
% 20 6
% 21 4