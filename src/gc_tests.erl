-module(gc_tests).
-include_lib("eunit/include/eunit.hrl").

gc_content_test() ->
	Filename = "../testdata/rosalind_gc.txt",
	ListOfDna = basic:open_fasta_file(Filename),
	Actual = gc:gc_content(ListOfDna),
	Expected = {"Rosalind_0808", 60.91954022988506},
	Actual = Expected,
	ok.