-module(dna_to_protein).
-export([translate/1]).

translate(Dna) ->
	translate(Dna, []).

translate([], ProteinString) ->
	lists:reverse(ProteinString);
translate(Dna, ProteinString) ->
	{Head, Rest} = lists:split(3, Dna),
	Protein = translate_one(Head),
	translate(Rest, [Protein | ProteinString]).

translate_one([dna_a, Char2, Char3]) ->
	translate_two_a(Char2, Char3);
translate_one([dna_c, Char2, Char3]) ->
	translate_two_c(Char2, Char3);
translate_one([dna_g, Char2, Char3]) ->
	translate_two_g(Char2, Char3);
translate_one([dna_t, Char2, Char3]) ->
	translate_two_t(Char2, Char3).

translate_two_a(dna_a, Char3) ->
	translate_three_aa(Char3);
translate_two_a(dna_c, Char3) ->
	translate_three_ac(Char3);
translate_two_a(dna_g, Char3) ->
	translate_three_ag(Char3);
translate_two_a(dna_t, Char3) ->
	translate_three_at(Char3).

translate_three_aa(dna_a) ->
	protein_k;
translate_three_aa(dna_c) ->
	protein_n;
translate_three_aa(dna_g) ->
	protein_k;
translate_three_aa(dna_t) ->
	protein_n.

translate_three_ac(_) ->
	protein_t.

translate_three_ag(dna_a) ->
	protein_r;
translate_three_ag(dna_c) ->
	protein_s;
translate_three_ag(dna_g) ->
	protein_r;
translate_three_ag(dna_t) ->
	protein_s.

translate_three_at(dna_g) ->
	protein_m;
translate_three_at(_) ->
	protein_i.


translate_two_g(dna_a, Char3) ->
	translate_three_ga(Char3);
translate_two_g(dna_c, _) ->
	protein_a;
translate_two_g(dna_g, _) ->
	protein_g;
translate_two_g(dna_t, _) ->
	protein_v.

translate_three_ga(dna_a) ->
	protein_e;
translate_three_ga(dna_c) ->
	protein_d;
translate_three_ga(dna_g) ->
	protein_e;
translate_three_ga(dna_t) ->
	protein_d.

translate_two_c(dna_a, Char3) ->
	translate_three_ca(Char3);
translate_two_c(dna_c, _) ->
	protein_p;
translate_two_c(dna_g, _) ->
	protein_r;
translate_two_c(dna_t, _) ->
	protein_l.

translate_three_ca(dna_a) ->
	protein_q;
translate_three_ca(dna_c) ->
	protein_h;
translate_three_ca(dna_g) ->
	protein_q;
translate_three_ca(dna_t) ->
	protein_h.

translate_two_t(dna_a, Char3) ->
	translate_three_ta(Char3);
translate_two_t(dna_c, _) ->
	protein_s;
translate_two_t(dna_g, Char3) ->
	translate_three_tg(Char3);
translate_two_t(dna_t, Char3) ->
	translate_three_tt(Char3).

translate_three_ta(dna_a) ->
	protein_stop;
translate_three_ta(dna_c) ->
	protein_y;
translate_three_ta(dna_g) ->
	protein_stop;
translate_three_ta(dna_t) ->
	protein_y.

translate_three_tg(dna_a) ->
	protein_stop;
translate_three_tg(dna_c) ->
	protein_c;
translate_three_tg(dna_g) ->
	protein_w;
translate_three_tg(dna_t) ->
	protein_c.

translate_three_tt(dna_a) ->
	protein_l;
translate_three_tt(dna_c) ->
	protein_f;
translate_three_tt(dna_g) ->
	protein_l;
translate_three_tt(dna_t) ->
	protein_f.