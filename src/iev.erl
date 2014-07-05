-module(iev).
-export([test/0, calculate/2]).

-spec calculate_AA_AA(integer(), integer()) -> float().
-spec calculate_AA_Aa(integer(), integer()) -> float().
-spec calculate_AA_aa(integer(), integer()) -> float().
-spec calculate_Aa_Aa(integer(), integer()) -> float().
-spec calculate_Aa_aa(integer(), integer()) -> float().
-spec calculate_aa_aa(integer(), integer()) -> float().


test() ->
	3.5 = calculate({1,0,0,1,0,1}, 2),
	152613.5 = calculate({18707, 17879, 16921, 19489, 16366, 18031}, 2),
	perfect.


calculate({Count_AA_AA, Count_AA_Aa, Count_AA_aa, Count_Aa_Aa, Count_Aa_aa, Count_aa_aa}, Number_of_offspings) ->
	Prob_count_AA_AA = calculate_AA_AA(Count_AA_AA, Number_of_offspings),
	Prob_count_AA_Aa = calculate_AA_Aa(Count_AA_Aa, Number_of_offspings),
	Prob_count_AA_aa = calculate_AA_aa(Count_AA_aa, Number_of_offspings),
	Prob_count_Aa_Aa = calculate_Aa_Aa(Count_Aa_Aa, Number_of_offspings),
	Prob_count_Aa_aa = calculate_Aa_aa(Count_Aa_aa, Number_of_offspings),
	Prob_count_aa_aa = calculate_aa_aa(Count_aa_aa, Number_of_offspings),
	Sum = Prob_count_AA_AA + Prob_count_AA_Aa + Prob_count_AA_aa + Prob_count_Aa_Aa + Prob_count_Aa_aa + Prob_count_aa_aa,
	Sum.


%AA-AA 1.0
calculate_AA_AA(Count, Number_of_offspings) ->
	Count * 1.0 * Number_of_offspings.

%AA-Aa 1.0
calculate_AA_Aa(Count, Number_of_offspings) ->
	Count * 1.0 * Number_of_offspings.

%AA-aa 1.0
calculate_AA_aa(Count, Number_of_offspings) ->
	Count * 1.0 * Number_of_offspings.

%Aa-Aa 0.75
calculate_Aa_Aa(Count, Number_of_offspings) ->
	Count * 0.75 * Number_of_offspings.

%Aa-aa 0.5
calculate_Aa_aa(Count, Number_of_offspings) ->
	Count * 0.5 * Number_of_offspings.

%aa-aa 0.0
calculate_aa_aa(Count, Number_of_offspings) ->
	Count * 0.0 * Number_of_offspings.