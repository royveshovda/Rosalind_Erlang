-module(dna).
-export([count/1]).

count(Dna) ->
	count(Dna,0,0,0,0).

count([], A, C, G, T) ->
	{count, A, C, G, T};
count([dna_a|Rest], A, C, G, T) ->
	count(Rest, A+1, C, G, T);
count([dna_c|Rest], A, C, G, T) ->
	count(Rest, A, C+1, G, T);
count([dna_g|Rest], A, C, G, T) ->
	count(Rest, A, C, G+1, T);
count([dna_t|Rest], A, C, G, T) ->
	count(Rest, A, C, G, T+1).