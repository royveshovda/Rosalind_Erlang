-module(types).

-type dna()			::	dna_a | dna_c | dna_g | dna_t.
-type dnas()		::	[dna()].
-type rna()			::	rna_a | rna_c | rna_g | rna_u.
-type rnas()		::	[rna()].
-type protein()		::	protein_c|protein_d|protein_e|
				   		protein_f|protein_g|protein_h|
				   		protein_i|protein_k|protein_l|
				   		protein_m|protein_n|protein_p|
				   		protein_q|protein_r|protein_s|
				   		protein_t|protein_v|protein_w|
				   		protein_y|protein_stop.
-type proteins()	:: 	[protein()].
-type dna_with_id()	:: {string(), dnas()}.

-export_type([dna/0, dnas/0, rna/0, rnas/0, protein/0, proteins/0, dna_with_id/0]).


