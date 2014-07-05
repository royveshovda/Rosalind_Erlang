-module(mprt).
-export([test/0, run/0]).

% get_ids_for_test() ->
% 	[
% 		"A2Z669",
% 		"B5ZC00",
% 		"P07204_TRBM_HUMAN",
% 		"P20840_SAG1_YEAST"
% 	].

get_ids() ->
	[
		"P05155_IC1_HUMAN",
		"B8GYE3",
		"Q07287_ZPB_PIG",
		"P01876_ALC1_HUMAN",
		"P07357_CO8A_HUMAN",
		"P19835_BAL_HUMAN",
		"P10761_ZP3_MOUSE",
		"B6DCT5",
		"P05783_K1CR_HUMAN",
		"B5ZC00",
		"P00750_UROT_HUMAN",
		"Q28409"
	].

get_expected() ->
	[
		{"P05155_IC1_HUMAN", [25, 69, 81, 238, 253, 352]},
		{"B8GYE3",[]},
		{"Q07287_ZPB_PIG", [70, 203, 220, 333, 474]}, 
		{"P01876_ALC1_HUMAN", [144, 340]}, 
		{"P07357_CO8A_HUMAN", [43, 437]},
		{"P19835_BAL_HUMAN", [207]}, 
		{"P10761_ZP3_MOUSE", [146, 273, 304, 327, 330]},
		{"B6DCT5",[]},
		{"P05783_K1CR_HUMAN", [193, 423]},
		{"B5ZC00", [85, 118, 142, 306, 395]},
		{"P00750_UROT_HUMAN", [152, 219, 483]},
		{"Q28409",[]}
	].

test() ->
	Ids = get_ids(),
	Match_list = match_all_proteins_with_n_glycosylation_motifs(Ids),
	%print_protein_match_lists(Match_list),
	Match_list = get_expected(),
	perfect.

run() ->
	Ids = get_ids(),
	Match_list = match_all_proteins_with_n_glycosylation_motifs(Ids),
	print_protein_match_lists(Match_list).

match_all_proteins_with_n_glycosylation_motifs(Protein_ids) ->
	Proteins = download_all(Protein_ids),
	List_of_matches = get_n_glycosylation_motifs(),
	Motifs_list = find_motifs_in_proteins(Proteins, List_of_matches),
	Motifs_list.

download_all(Ids) ->
	download_all(Ids, []).

download_all([], Results) ->
	Results;
download_all([Id|Rest_of_ids], Results) ->
	Result = download_uniprot_fasta(Id),
	download_all(Rest_of_ids, [Result | Results]).


%Most likely a redirect here
%http://www.uniprot.org/uniprot/P81448_EMBP_CRIGR.fasta
%httpc:request(get, {"http://www.uniprot.org/uniprot/P81448_EMBP_CRIGR.fasta",[]}, [{autoredirect, true}])

%{ok, {{_Version, 302, _StatusMessage}, Headers, _Body}}
%{value, {_Key, NewLocation}, _NewList} = lists:keytake("location", 1,Headers).


download_uniprot_fasta(Id) ->
	Host = "http://www.uniprot.org",
	PreString = "/uniprot/",
	PostString = ".fasta",
	Location = PreString ++ Id ++ PostString,
	Body = download(Host, {location, Location}),
	BinaryBody = list_to_binary(Body),
	[BinaryId, BinaryContent] = binary:split(BinaryBody, <<$\n>>),
	%{BinaryId, BinaryContent}.
	Raw_Id = binary:bin_to_list(BinaryId),
 	LongId = basic:remove_line_breaks(Raw_Id),
 	StringContent = binary:bin_to_list(BinaryContent),
 	TrimmedString = basic:remove_line_breaks(StringContent),
 	Content = convert:string_to_protein(TrimmedString),
 	{Id, LongId, Content}.


 	download(Host, {location, Location}) ->
 		download(Host, {url, Host ++ Location});
 	download(Host, {url, Url}) ->
 		start_inets(),
 		case httpc:request(get, {Url,[]}, [{autoredirect, false}], []) of
			{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
				Body;
			{ok, {{_Version, 302, _StatusMessage}, Headers, _Body}} ->
				{value, {_Key, New_url}, _NewList} = lists:keytake("location", 1,Headers),
				download(Host, {url, New_url});
			{ok, {{_Version, 301, _StatusMessage}, Headers, _Body}} ->
				{value, {_Key, NewLocation}, _NewList} = lists:keytake("location", 1,Headers),
				download(Host, {location, NewLocation})
		end.


 start_inets() ->
 	Res = inets:start(),
 	return_start_result(Res).

 return_start_result(ok) ->
 	ok;
 return_start_result({error,{already_started,_}}) ->
 	ok;
 return_start_result({error, Reason}) ->
 	{error, Reason}.

find_motifs_in_proteins(Proteins, List_of_matches) ->
	find_motifs_in_proteins(Proteins, List_of_matches, []).

find_motifs_in_proteins([], _List_of_matches, Results) ->
	Results;
find_motifs_in_proteins([{Id, _Long_Id, Protein}|Rest_of_proteins], List_of_matches, Results) ->
	Motifs = find_motifs_in_protein(Protein, List_of_matches),
	find_motifs_in_proteins(Rest_of_proteins, List_of_matches, [{Id, Motifs} | Results]).

find_motifs_in_protein(Protein, Matching_protein_list) ->
	find_motifs_in_protein(Protein, Matching_protein_list, []).

find_motifs_in_protein(_Protein, [], Findings) ->
	lists:sort(Findings);
find_motifs_in_protein(Protein, [Match| Rest_of_matching_protein_list], Findings) ->
	NewFindings = subs:motif(Protein, Match),
	AllFindings = Findings ++ NewFindings,
	find_motifs_in_protein(Protein, Rest_of_matching_protein_list, AllFindings).

print_protein_match_lists([]) ->
	ok;
print_protein_match_lists([{_Id, Match_list}|Rest]) when length(Match_list) =< 0 ->
	print_protein_match_lists(Rest);
print_protein_match_lists([{Id, Match_list}|Rest]) when length(Match_list) > 0 ->
	io:format("~s~n", [Id]),
	print_list(Match_list),
	print_protein_match_lists(Rest).

print_list([]) ->
	io:format("~n");
print_list([Position|Rest]) ->
	io:format("~p ", [Position]),
	print_list(Rest).


%N{P}[ST]{P}


get_all_proteins_but_p() ->
	[protein_a,protein_c,protein_d,protein_e,protein_f,protein_g,
	 protein_h,protein_i,protein_k,protein_l,protein_m,protein_n,
	 protein_q,protein_r,protein_s,protein_t,protein_v,
	 protein_w,protein_y].

get_n_glycosylation_motifs() ->
	P1 = get_all_proteins_but_p(),
	P2 = get_all_proteins_but_p(),
	ST = [protein_s, protein_t],
	[[protein_n, Prot1, S_or_T, Prot2] || Prot1 <- P1, S_or_T <- ST, Prot2 <- P2].