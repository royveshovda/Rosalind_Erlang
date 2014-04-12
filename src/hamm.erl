-module(hamm).
-export([hamming_distance/2]).

hamming_distance(Dna1, Dna2) ->
	hamming_distance(Dna1, Dna2, 0).

hamming_distance([], [], Count) ->
	Count;
hamming_distance([H1|T1], [H2|T2], Count) ->
	if
		H1 =:= H2 -> hamming_distance(T1, T2, Count);
		H1 =/= H2 -> hamming_distance(T1, T2, Count +1)
	end.