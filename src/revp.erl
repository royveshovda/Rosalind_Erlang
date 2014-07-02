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
	F4 = get_new_findings_based_on_length(Position, 4, Dna, Findings),
	F6 = get_new_findings_based_on_length(Position, 6, Dna, F4),
	F8 = get_new_findings_based_on_length(Position, 8, Dna, F6),
	F10 = get_new_findings_based_on_length(Position, 10, Dna, F8),
	F12 = get_new_findings_based_on_length(Position, 12, Dna, F10),
	search(Dna, Position+1, F12).

get_new_findings_based_on_length(Position, Length, Dna, Findings) when (Position + Length) =< (length(Dna)+1) ->
	SubList = lists:sublist(Dna,Position,Length),
	Is = is_revese_palindrome(SubList),
	New_Findings = get_new_findings(Is, Findings, Position, Length),
	New_Findings;
get_new_findings_based_on_length(_Position, _Length, _Dna, Findings) when (_Position + _Length) > (length(_Dna)+1) ->
	Findings.



get_new_findings(true, Findings, Position, Length) ->
	[{Position, Length} | Findings];
get_new_findings(false, Findings, _Position, _Length) ->
	Findings.

%add_to_findings(true, Position, )

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