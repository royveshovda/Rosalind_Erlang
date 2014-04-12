-module(gc).
-export([gc_content/1]).

gc_content(DnaList) ->
	CountedData = count_data(DnaList, []),
	max_gc_sum(CountedData).

count_data([], Counted) ->
	Counted;
count_data([{Id, Dna} | Rest], Counted) ->
	Sum = dna:count(Dna),
	Gc = make_gc_sum(Sum),
	count_data(Rest, [{Id, Gc} | Counted]).

max_gc_sum([{Id, Sum} | SumDataTail]) ->
	max_gc_sum(SumDataTail, Id, Sum).

max_gc_sum([], MaxId, MaxSum) ->
	{MaxId, MaxSum};
max_gc_sum([{Id, Sum} | SumDataTail], MaxId, MaxSum) ->
	if
		Sum >= MaxSum -> max_gc_sum(SumDataTail, Id, Sum);
		Sum < MaxSum -> max_gc_sum(SumDataTail, MaxId, MaxSum)
	end.

make_gc_sum({count, 0, 0, 0, 0}) ->
	0;
make_gc_sum({count, A, C, G, T}) ->
	((G+C) / (A+C+G+T))*100.0.