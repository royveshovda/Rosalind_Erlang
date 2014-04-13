-module(grph).
-export([find_direct_graph/2, test/0]).

test()->
	Filename = "../testdata/rosalind_grph2.txt",
	ListOfDnas = basic:open_fasta_file(Filename),
	Graph = find_direct_graph(ListOfDnas, 3),
	print_graph_list(Graph).

find_direct_graph(ListOfDnas, Overlap_count) ->
	find_direct_graph(ListOfDnas, Overlap_count, []).

find_direct_graph([_Head|[]], _Overlap_count, Graph) ->
	lists:reverse(Graph);
find_direct_graph([{Id, Dna} | ListOfDnas], Overlap_count, Graph) ->
	New_Graph = find_graph_matches(Id, Dna, ListOfDnas, Overlap_count, Graph),
	find_direct_graph(ListOfDnas, Overlap_count, New_Graph).

find_graph_matches(_Id, _Dna, [], _Overlap_count, Graph) ->
	Graph;
find_graph_matches(Id, Dna, [{Id2, Dna2} | ListOfDnas], Overlap_count, Graph) ->
	New_Graph = match_dnas(Id, Dna, Id2, Dna2, Overlap_count, Graph),	
	find_graph_matches(Id, Dna, ListOfDnas, Overlap_count, New_Graph).

match_dnas(Id1, Dna1, Id2, Dna2, Overlap_count, Graph) ->
	Match1 = is_direct_graph(Dna1, Dna2, Overlap_count),
	if
		Match1 == true ->
			New_Graph1 = [{Id2, Id1} | Graph];
		Match1 == false ->
			New_Graph1 = Graph
	end,

	Match2 = is_direct_graph(Dna2, Dna1, Overlap_count),
	if
		Match2 == true ->
			New_Graph2 = [{Id1, Id2} | New_Graph1];
		Match2 == false ->
			New_Graph2 = New_Graph1
	end,
	New_Graph2.

is_direct_graph(Dna1, Dna2, Overlap_count) ->
	Sub_Dna1 = lists:sublist(Dna1, Overlap_count),
	Sub_Dna2_1 = lists:reverse(Dna2),
	Sub_Dna2_2 = lists:sublist(Sub_Dna2_1, Overlap_count),
	Sub_Dna2 = lists:reverse(Sub_Dna2_2),
	compare_dnas(Sub_Dna1, Sub_Dna2).

compare_dnas([], []) ->
	true;
compare_dnas([Head1 | Tail1], [Head2 | Tail2]) ->
	if
		Head1 =/= Head2 ->
			false;
		Head1 == Head2 ->
			compare_dnas(Tail1, Tail2)
	end.

print_graph_list([]) ->
	ok;
print_graph_list([{Id1, Id2} | Rest_of_graph]) ->
	io:format("~s ~s~n",[Id1, Id2]),
	print_graph_list(Rest_of_graph).