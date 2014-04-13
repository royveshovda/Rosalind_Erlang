-module(cons).
-export([find_consensus/1, test/0]).

test() ->
	Filename = "../testdata/rosalind_cons2.txt",
	ListOfDnas = basic:open_fasta_file(Filename),
	find_consensus(ListOfDnas).

find_consensus(ListOfDnas) ->
	Matrixes = filter_names_from_dna_in_matrix(ListOfDnas, []),
	Analyzed_matrix = create_dna_sum_for_matrix(Matrixes),
	Matrix_Sum = sum_matrixes(Analyzed_matrix),
	Consensus = create_consensus(Matrix_Sum),
	basic:print_dna(Consensus),
	print_matrix_sum(Matrix_Sum).


filter_names_from_dna_in_matrix([], Matrix) ->
	lists:reverse(Matrix);
filter_names_from_dna_in_matrix([{_Name, Dna}| ListOfDnas], Matrix) ->
	filter_names_from_dna_in_matrix(ListOfDnas, [Dna | Matrix]).


get_length_of_matrix([{A, _C, _G, _T} | _Rest_of_Matrix]) ->
	length(A).

sum_matrixes(Matrixes) ->
	Length = get_length_of_matrix(Matrixes),
	Empty_sum = create_empty_result_matrix(Length),
	sum_matrixes(Matrixes, Empty_sum).

create_dna_sum_for_matrix(Matrix) ->
	create_dna_sum_for_matrix(Matrix, []).

create_dna_sum_for_matrix([], Sum) ->
	lists:reverse(Sum);
create_dna_sum_for_matrix([Dna|List_of_dnas], Sum) ->
	Dna_sum = analyze_DNA(Dna),
	create_dna_sum_for_matrix(List_of_dnas, [Dna_sum | Sum]).

sum_matrixes([], Sum) ->
	Sum;
sum_matrixes([Matrix_Head| Matrix_Tail], Sum) ->
	New_Sum = sum_two_matrixes(Matrix_Head, Sum),
	sum_matrixes(Matrix_Tail, New_Sum).


create_empty_result_matrix(Length) -> 
	A = create_empty_result_matrix(0, Length, []),
	C = create_empty_result_matrix(0, Length, []),
	G = create_empty_result_matrix(0, Length, []),
	T = create_empty_result_matrix(0, Length, []),
	{A,C,G,T}.

create_empty_result_matrix(Length, Length, Matrix) ->
	lists:reverse(Matrix);
create_empty_result_matrix(Position, Length, Matrix) ->
	create_empty_result_matrix(Position+1, Length, [0|Matrix]).

create_consensus(Matrix) ->
	create_consensus(Matrix, []).

create_consensus({[], [], [], []}, Cons) ->
	lists:reverse(Cons);
create_consensus({[Head_A|Tail_A], [Head_C|Tail_C], [Head_G|Tail_G], [Head_T|Tail_T]}, Cons) -> 
	Largest = find_largest(Head_A, Head_C, Head_G, Head_T),
	create_consensus({Tail_A, Tail_C, Tail_G, Tail_T}, [Largest | Cons]).

find_largest(A, C, G, T) when A>=C , A>=G, A>=T ->
	dna_a;
find_largest(A, C, G, T) when C>A , C>=G, C>=T ->
	dna_c;
find_largest(A, C, G, T) when G>A , G>C, G>=T ->
	dna_g;
find_largest(A, C, G, T) when T>A , T>C, T>G ->
	dna_t.

analyze_DNA(Dna) ->
	analyze_DNA(Dna, {[], [], [], []}).

analyze_DNA([], {A,C,G,T}) ->
	{lists:reverse(A), lists:reverse(C), lists:reverse(G), lists:reverse(T)};
analyze_DNA([dna_a | Dna], {A,C,G,T}) ->
	analyze_DNA(Dna, {[1|A], [0|C], [0|G], [0|T]});
analyze_DNA([dna_c | Dna], {A,C,G,T}) ->
	analyze_DNA(Dna, {[0|A], [1|C], [0|G], [0|T]});
analyze_DNA([dna_g | Dna], {A,C,G,T}) ->
	analyze_DNA(Dna, {[0|A], [0|C], [1|G], [0|T]});
analyze_DNA([dna_t | Dna], {A,C,G,T}) ->
	analyze_DNA(Dna, {[0|A], [0|C], [0|G], [1|T]}).

sum_two_lists(List1, List2) ->
	sum_two_lists(List1, List2, []).

sum_two_lists([], [], Sum) ->
	lists:reverse(Sum);
sum_two_lists([Head1 | Tail1], [Head2 | Tail2], Sum) ->
	sum_two_lists(Tail1,Tail2, [Head1+Head2 | Sum]).

sum_two_matrixes({A1, C1, G1,T1}, {A2, C2, G2, T2}) ->
	A = sum_two_lists(A1, A2),
	C = sum_two_lists(C1, C2),
	G = sum_two_lists(G1, G2),
	T = sum_two_lists(T1, T2),
	{A, C, G, T}.

print_matrix_sum({A, C, G, T}) ->
	io:format("A:", []),
	print_sum(A),
	io:format("~nC:", []),
	print_sum(C),
	io:format("~nG:", []),
	print_sum(G),
	io:format("~nT:", []),
	print_sum(T),
	io:format("~n", []).

print_sum([]) ->
	ok;
print_sum([Head | Tail]) ->
	io:format(" ~p",[Head]),
	print_sum(Tail).









