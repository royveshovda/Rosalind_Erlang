-module(subs).
-export([motif/2]).

%motif(MainString, SubString) ->
%	motif([$X|MainString], SubString, [], 0).

%motif([], _, Temp, _) ->
%	lists:reverse(Temp);
%motif([_|RestOfMainString], SubString, Temp, Shift) ->
%	Pos = string:str(RestOfMainString, SubString),
%	if
%		Pos =< 0 -> lists:reverse(Temp);
%		Pos >= 1 -> motif(string:substr(RestOfMainString, Pos), SubString, [Shift+Pos|Temp], Shift+Pos)
%	end.

motif(Content, Match) ->
	motif(Content, Match, [], 1).

motif([], _Match, MatchList, _Position) ->
	lists:reverse(MatchList);
motif(Content, Match, MatchList, Position) ->
	LengthMatch = length(Match),
	LengthContent = length(Content),
	if
		LengthContent < LengthMatch -> motif([], Match, MatchList, Position+LengthContent);
		LengthContent >= LengthMatch ->
			SubContent = lists:sublist(Content, LengthMatch),
			Diff = hamm:hamming_distance(SubContent, Match),
			NextList = lists:sublist(Content,2,LengthContent),
			if
				Diff =:= 0 -> motif(NextList, Match, [Position | MatchList], Position+1);
				Diff =/= 0 -> motif(NextList, Match, MatchList, Position+1)
			end
	end.
	