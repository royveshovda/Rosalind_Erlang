-module(main).
-export([init/0, stop/0, count_dna/1, loop/0, get_test/0, convert_dna_string/1]).
-define(ROSALIND, rosalind_main).

init() ->
	Pid = spawn(?MODULE, loop, []),
	true = register(?ROSALIND, Pid),
	ok.

get_pid() ->
	whereis(?ROSALIND).

stop() ->
	Pid = get_pid(),
	case Pid of
		undefined ->
			io:format("Already stopped~n",[]);
		_ -> 
			Pid ! stop
	end,
	ok.

count_dna(DnaList) ->
	Pid = get_pid(),
	Ref = make_ref(),
	Pid ! {self(), Ref, dna, count, DnaList},
	receive
		{dna_counted, Ref, Result} ->
		Result
	end.

convert_dna_string(DnaString) ->
	Pid = get_pid(),
	Ref = make_ref(),
	Pid ! {self(), Ref, dna, convert_string, DnaString},
	receive
		{dna_converted, Ref, Result} ->
		Result
	end.	

loop() ->
	receive
		stop ->
			ok;
		{Pid, Ref, dna, count, DnaList} ->
			Result = {counted, _A, _C, _G, _T} = dna:count(DnaList),
			Pid ! {dna_counted, Ref, Result},
			loop();
		{Pid, Ref, dna, convert_string, DnaString} ->
			Result = basic:string_to_dna(DnaString),
			Pid ! {dna_converted, Ref, Result},
			loop()
	end.