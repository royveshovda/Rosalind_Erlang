-module(mrna).
-export([test/0, calculate_rna_possibilities/1]).


test() ->
	ProteinString = "MA.",
	Protein = basic:string_to_protein(ProteinString),
	12 = calculate_rna_possibilities(Protein),
	ProteinString2 = "MPKQICSKLQGGHTTWNKFQMMCMCEDTPGHCSQVDPNGEWFAKCIDFEITVCGVYLGEFNSLASGEGTWMDSPVVRVDNFIHEHNKDFIDWISDHNDFTGKVCPIRLTCSQYWHECNHCRHELWCWREWHHRKNGAKVCYWKHISNNSFKFYNPNETDHDQDDPGVHLSKWHDLYHIPTVHMVQVRRFVKWLMHYDKRTNLDIEQGTVFGAVKRQTWSGCIDERCYGAKYFCIIWNAMYEKIYSDWYSQGDQRTPDRGWNSVWSKLTDTIEGGEVLQFNKGRSLVHFAFSDRCVFAMCDRNIGLDDKLHNAVCPEDTPIPKTDLEEQPSQYTIMGDFIEITKCHTPNQTKRVNYNNKFFYGILQAFCHQMPSWPIRQDISYILQLKRLDHDPTVNKWTLYCWICWNRCRCVGWQRLQNHEFFKAQRHICCGRVEYLNYNGCLLFQIIGFGFLYDFFTRNKQLVWNHHQWFGAEHIRELWAPKAQMQVFFNKLRAHDGGHWHIQGMQGDQQWVSMFPRICACNKIEDTWWKVETLNAVINIKRHPPLNDIPPIDKPHALVPGKLYAPWSSYLLVCAYNHMVRMTSHLSLYLHDMDEQQGFRFWTYYVSYIAEASMEEALQFHVMFYVDKNLAQNDEKWDRQYCEMWFWASHTNVRLSMKHYSHMLDNGYDHWSMYLIGVGFQPWSDKFNMTHAHKLTCQYEPTCPQYRQAIKGMQNSRVFDESMGFLHLHKVWMWIFRMSCDKRTPWIRHKQVNAFMGIVEDSDLTPVASTDVFIASCQAETSLLYGTMNTHNVDWAKAQKECMPMMIVHYQRKLTIWDRFKSHQVASIKAVNQTCCFTRVGNIYYCLFWHIHHNACCCLKARLVIESHGSPHYDPCYRAQLYGPVVIIDRILSDHGPECKGTKMCMCSTRQDALIARMIQSHCDCSSGPLLWMVWWRVFAQYVERVHHMPGYWTIGPQCVIPYLICMIWTAALTHRVHIRTCRARVNKHLGLYDMQTR.",
	Protein2 = basic:string_to_protein(ProteinString2),
	679104 = calculate_rna_possibilities(Protein2),
	ok.

calculate_rna_possibilities(Protein) ->
	calculate_rna_possibilities(Protein, 1).

calculate_rna_possibilities([], Sum) ->
	Sum;
calculate_rna_possibilities([Protein|Rest], Sum) ->
	ProteinWeight = get_number_of_possible_rna_from_protein(Protein),
	NewSum = ProteinWeight * Sum,
	ModuloSum = NewSum rem 1000000,
	% fix calculation to include mod 1 000 000
	calculate_rna_possibilities(Rest, ModuloSum).


get_number_of_possible_rna_from_protein(protein_f) ->
	2;
get_number_of_possible_rna_from_protein(protein_k) ->
	2;
get_number_of_possible_rna_from_protein(protein_n) ->
	2;
get_number_of_possible_rna_from_protein(protein_t) ->
	4;
get_number_of_possible_rna_from_protein(protein_r) ->
	6;
get_number_of_possible_rna_from_protein(protein_s) ->
	6;
get_number_of_possible_rna_from_protein(protein_m) ->
	1;
get_number_of_possible_rna_from_protein(protein_i) ->
	3;
get_number_of_possible_rna_from_protein(protein_p) ->
	4;
get_number_of_possible_rna_from_protein(protein_l) ->
	6;
get_number_of_possible_rna_from_protein(protein_q) ->
	2;
get_number_of_possible_rna_from_protein(protein_h) ->
	2;
get_number_of_possible_rna_from_protein(protein_a) ->
	4;
get_number_of_possible_rna_from_protein(protein_g) ->
	4;
get_number_of_possible_rna_from_protein(protein_v) ->
	4;
get_number_of_possible_rna_from_protein(protein_e) ->
	2;
get_number_of_possible_rna_from_protein(protein_d) ->
	2;
get_number_of_possible_rna_from_protein(protein_y) ->
	2;
get_number_of_possible_rna_from_protein(protein_c) ->
	2;
get_number_of_possible_rna_from_protein(protein_w) ->
	1;
get_number_of_possible_rna_from_protein(protein_stop) ->
	3.





%Sum: 64