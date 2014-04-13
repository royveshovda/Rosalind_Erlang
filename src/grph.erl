-module(grph).
-export([find_direct_graph/2, test/0]).

test()->
	Filename = "../testdata/rosalind_grph.txt",
	ListOfDnas = basic:open_fasta_file(Filename),
	find_direct_graph(ListOfDnas, 3).

find_direct_graph(ListOfDnas, Overlap_count) ->
	find_direct_graph(ListOfDnas, Overlap_count, []).

find_direct_graph([_Head|[]], Overlap_count, Graph) ->
	Graph;
find_direct_graph([{Id, Dna} | ListOfDnas], Overlap_count, Graph) ->
	New_Graph = find_graph_matches(Id, Dna, ListOfDnas, Overlap_count, Graph),
	find_direct_graph(ListOfDnas, Overlap_count, New_Graph).
	%1: loop all Heads
	%2: compare with each of the others
	%3: If match, save graph

find_graph_matches(Id, Dna, [], Overlap_count, Graph) ->
	lists:reverse(Graph);
find_graph_matches(Id, Dna, [{Id2, Dna2} | ListOfDnas], Overlap_count, Graph) ->
	

	%TODO: Compare, and only add if match

	%TODO: Compare both directions
	
	New_Graph1 = [{Id, Id2} | Graph],
	New_Graph2 = [{Id2, Id} | New_Graph1],
	find_graph_matches(Id, Dna, ListOfDnas, Overlap_count, New_Graph2).

