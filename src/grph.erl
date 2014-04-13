-module(grph).
-export([find_direct_graph/1, test/0]).

test()->
	Filename = "../testdata/rosalind_grph.txt",
	ListOfDnas = basic:open_fasta_file(Filename),
	find_direct_graph(ListOfDnas).

find_direct_graph(ListOfDnas) ->
	ListOfDnas.