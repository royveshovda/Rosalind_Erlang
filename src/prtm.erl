-module(prtm).
-export([test/0, run/0]).

-spec get_monoisotopic_mass(types:protein()) -> float().
-spec calculate_monoisotopic_mass(types:proteins()) -> float().

run() ->
	Protein_string =	"QRDDCVSHMEKSIGHFHYIMGNSYPDCTFWEQYRTAWWQHCMYWKFEEYPNNMMCPERIKAALAIPGMNKWETFRDVPCQ
						 WQTEDCQLTNMSVLLHYQFRNWKKEKGMFRHDLDGELIGTAEHLEPEQDQCVVNSCYSLNQSISPDHQDDLQWDTRQTDI
						 KFPHIANRARMMEYMKEPWVSSGVNVAYPCSERTPGQLNQEPRLEVICGRGYEFFTVLLSNLGTFTMTVGLAEMDNQDHS
						 PYFPCLQKMTIWLHNTSLSSAVMENWDRDPNVQVWYCEISRFRSTWMHDWYDYGKCGYHMILHVSRADCWPAMTDIMWAH
						 TDTNVYASTACTCHIPAAEGMQMGQRGVRKHFEQFWGCYKELESYMRYIQCLAHEGLGVYYIAYKLKIQMMCSGFMWGLN
						 WQIGYIEMPAGHTIQGLYAGDNVMVPEPYGVATSAHERQRRAIFGARTGDAFPSWIMPIYPVIWPLKRNALRKADMSMKS
						 PGHLSWYLICEMPVHQIMQKGIETGALDNWWWGMDAWILYCSATEKPMHDSEQKKCLTAYRRPNYGTQATIWPQYAMPIE
						 HSFGAQPSDTVWIKKLWQPCDCWRMCCFDSYNPRWAVACYHITSNHLNHDESTIERDNQRQFNHEIPKDYPSPIWAMHTD
						 IRFVRHSPIRLHGCGWCASKIWSYCDCATKSVFADRWPMTAQATAASWLLRFAKAHHQALILNFLPKRTQETCSISVWMF
						 VTFHALWHMQEIKCRSHWVNVSYTEYNGSIVKQNKYLHFCDPIYPAAYPDHQPEKHQTRGPCEPPWCLPSWMDNHQVIEI
						 SSHGPFRQLILHEDSVGRPMRV",
	Proteins = convert:string_to_protein(Protein_string),
	calculate_monoisotopic_mass(Proteins).

test() ->
	Protein_string ="SKADYEK",
	Proteins = convert:string_to_protein(Protein_string),
	821.3919199999999 = calculate_monoisotopic_mass(Proteins),
	perfect.


calculate_monoisotopic_mass(Proteins) ->
	calculate_monoisotopic_mass(Proteins, 0).

calculate_monoisotopic_mass([], Sum) ->
	Sum;
calculate_monoisotopic_mass([Protein | Rest], Sum) ->
	Weight = get_monoisotopic_mass(Protein),
	calculate_monoisotopic_mass(Rest, Sum+Weight).


get_monoisotopic_mass(protein_a) ->
	71.03711;
get_monoisotopic_mass(protein_c) ->
	103.00919;
get_monoisotopic_mass(protein_d) ->
	115.02694;
get_monoisotopic_mass(protein_e) ->
	129.04259;
get_monoisotopic_mass(protein_f) ->
	147.06841;
get_monoisotopic_mass(protein_g) ->
	57.02146;
get_monoisotopic_mass(protein_h) ->
	137.05891;
get_monoisotopic_mass(protein_i) ->
	113.08406;
get_monoisotopic_mass(protein_k) ->
	128.09496;
get_monoisotopic_mass(protein_l) ->
	113.08406;
get_monoisotopic_mass(protein_m) ->
	131.04049;
get_monoisotopic_mass(protein_n) ->
	114.04293;
get_monoisotopic_mass(protein_p) ->
	97.05276;
get_monoisotopic_mass(protein_q) ->
	128.05858;
get_monoisotopic_mass(protein_r) ->
	156.10111;
get_monoisotopic_mass(protein_s) ->
	87.03203;
get_monoisotopic_mass(protein_t) ->
	101.04768;
get_monoisotopic_mass(protein_v) ->
	99.06841;
get_monoisotopic_mass(protein_w) ->
	186.07931;
get_monoisotopic_mass(protein_y) ->
	163.06333.