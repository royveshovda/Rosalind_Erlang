-module(fib).
-export([wabbits/2]).

wabbits(1, _OffspringPerGeneration) ->
	1;
wabbits(2, _OffspringPerGeneration) ->
	1;
wabbits(Months, OffspringPerGeneration) ->
	wabbits(Months-1, OffspringPerGeneration) + (OffspringPerGeneration * wabbits(Months-2, OffspringPerGeneration)).