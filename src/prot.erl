-module(prot).
-export([rna_to_protein/1]).

-spec rna_to_protein(types:rnas()) -> types:proteins().

rna_to_protein(Rna) ->
	rna_to_protein(Rna, []).

rna_to_protein([], ProteinString) ->
	lists:reverse(ProteinString);
rna_to_protein(Rna, ProteinString) ->
	{Head, Rest} = lists:split(3, Rna),
	Protein = translate_one(Head),
	rna_to_protein(Rest, [Protein | ProteinString]).



%get_translate_table() ->
%	[
%		{"AAA", "K"}, {"AAC", "N"}, {"AAG", "K"}, {"AAU", "N"}, 
%     	{"ACA", "T"}, {"ACC", "T"}, {"ACG", "T"}, {"ACU", "T"},
%		{"AGA", "R"}, {"AGC", "S"}, {"AGG", "R"}, {"AGU", "S"},
%      	{"AUA", "I"}, {"AUC", "I"}, {"AUG", "M"}, {"AUU", "I"},
%		{"CAA", "Q"}, {"CAC", "H"}, {"CAG", "Q"}, {"CAU", "H"},
%		{"CCA", "P"}, {"CCC", "P"}, {"CCG", "P"}, {"CCU", "P"},
%		{"CGA", "R"}, {"CGC", "R"}, {"CGG", "R"}, {"CGU", "R"},
%		{"CUA", "L"}, {"CUC", "L"}, {"CUG", "L"}, {"CUU", "L"},
%		{"GAA", "E"}, {"GAC", "D"}, {"GAG", "E"}, {"GAU", "D"},
%     	{"GCA", "A"}, {"GCC", "A"}, {"GCG", "A"}, {"GCU", "A"},
%      	{"GGA", "G"}, {"GGC", "G"}, {"GGG", "G"}, {"GGU", "G"},
%     	{"GUA", "V"}, {"GUC", "V"}, {"GUG", "V"}, {"GUU", "V"},		
%		{"UAA", stop}, {"UAC", "Y"}, {"UAG", stop}, {"UAU", "Y"},
%		{"UCA", "S"}, {"UCC", "S"}, {"UCG", "S"}, {"UCU", "S"},
%		{"UGA", stop}, {"UGC", "C"}, {"UGG", "W"}, {"UGU", "C"},
%		{"UUA", "L"}, {"UUC", "F"}, {"UUG", "L"}, {"UUU", "F"}
%	].

translate_one([rna_a, Char2, Char3]) ->
	translate_two_a(Char2, Char3);
translate_one([rna_c, Char2, Char3]) ->
	translate_two_c(Char2, Char3);
translate_one([rna_g, Char2, Char3]) ->
	translate_two_g(Char2, Char3);
translate_one([rna_u, Char2, Char3]) ->
	translate_two_u(Char2, Char3).
	

translate_two_a(rna_a, Char3) ->
	translate_three_aa(Char3);
translate_two_a(rna_c, Char3) ->
	translate_three_ac(Char3);
translate_two_a(rna_g, Char3) ->
	translate_three_ag(Char3);
translate_two_a(rna_u, Char3) ->
	translate_three_au(Char3).


translate_three_aa(rna_a) ->
	protein_k;
translate_three_aa(rna_c) ->
	protein_n;
translate_three_aa(rna_g) ->
	protein_k;
translate_three_aa(rna_u) ->
	protein_n.

translate_three_ac(_) ->
	protein_t.

translate_three_ag(rna_a) ->
	protein_r;
translate_three_ag(rna_c) ->
	protein_s;
translate_three_ag(rna_g) ->
	protein_r;
translate_three_ag(rna_u) ->
	protein_s.

translate_three_au(rna_g) ->
	protein_m;
translate_three_au(_) ->
	protein_i.


translate_two_c(rna_a, Char3) ->
	translate_three_ca(Char3);
translate_two_c(rna_c, _) ->
	protein_p;
translate_two_c(rna_g, _) ->
	protein_r;
translate_two_c(rna_u, _) ->
	protein_l.


translate_three_ca(rna_a) ->
	protein_q;
translate_three_ca(rna_c) ->
	protein_h;
translate_three_ca(rna_g) ->
	protein_q;
translate_three_ca(rna_u) ->
	protein_h.


translate_two_g(rna_a, Char3) ->
	translate_three_ga(Char3);
translate_two_g(rna_c, _) ->
	protein_a;
translate_two_g(rna_g, _) ->
	protein_g;
translate_two_g(rna_u, _) ->
	protein_v.

translate_three_ga(rna_a) ->
	protein_e;
translate_three_ga(rna_c) ->
	protein_d;
translate_three_ga(rna_g) ->
	protein_e;
translate_three_ga(rna_u) ->
	protein_d.

translate_two_u(rna_a, Char3) ->
	translate_three_ua(Char3);
translate_two_u(rna_c, _) ->
	protein_s;
translate_two_u(rna_g, Char3) ->
	translate_three_ug(Char3);
translate_two_u(rna_u, Char3) ->
	translate_three_uu(Char3).

translate_three_ua(rna_a) ->
	protein_stop;
translate_three_ua(rna_c) ->
	protein_y;
translate_three_ua(rna_g) ->
	protein_stop;
translate_three_ua(rna_u) ->
	protein_y.

translate_three_ug(rna_a) ->
	protein_stop;
translate_three_ug(rna_c) ->
	protein_c;
translate_three_ug(rna_g) ->
	protein_w;
translate_three_ug(rna_u) ->
	protein_c.

translate_three_uu(rna_a) ->
	protein_l;
translate_three_uu(rna_c) ->
	protein_f;
translate_three_uu(rna_g) ->
	protein_l;
translate_three_uu(rna_u) ->
	protein_f.
