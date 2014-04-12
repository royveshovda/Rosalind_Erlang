-module(test).
-export([test/0]).

test() ->
	eunit:test(dna),
	eunit:test(rna),
	eunit:test(revc),
	eunit:test(gc),
	eunit:test(hamm),
	eunit:test(prot),
	eunit:test(subs),
	eunit:test(fib),
	eunit:test(iprb).