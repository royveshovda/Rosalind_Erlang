-module(fibd).
-export([wabbits/2]).

wabbits(Months, MonthsEachRabbitWillLive) ->
	L = [{2,1}, {1,1}, {0,0}],
	{Value, _Dict} = wabbits2(Months,MonthsEachRabbitWillLive, L),
	Value.

wabbits2(Months, MonthsEachRabbitWillLive, PropList) when Months >= MonthsEachRabbitWillLive ->
	KeyExisting = proplists:is_defined(Months, PropList),
	wabbits3(Months, MonthsEachRabbitWillLive, KeyExisting, PropList);
wabbits2(Months, MonthsEachRabbitWillLive, PropList) when Months < MonthsEachRabbitWillLive ->
	wabbits2(Months, Months, PropList).

wabbits3(Months, _MonthsEachRabbitWillLive, true, PropList) ->
	Value = proplists:get_value(Months,PropList),
	{Value, PropList};
wabbits3(Months, MonthsEachRabbitWillLive, false, PropList) when Months < MonthsEachRabbitWillLive + 1 ->
	{ValueN2 , PropListN2} = wabbits2(Months-2, MonthsEachRabbitWillLive, PropList),
	{ValueN1, PropListN1} = wabbits2(Months-1, MonthsEachRabbitWillLive, PropListN2),
	Value = ValueN1 + ValueN2,
	PropListRet = [{Months, Value} | PropListN1],
	{Value, PropListRet};
wabbits3(Months, MonthsEachRabbitWillLive, false, PropList) when Months =:= MonthsEachRabbitWillLive + 1 ->
	{ValueN2 , PropListN2} = wabbits2(Months-2, MonthsEachRabbitWillLive, PropList),
	{ValueN1, PropListN1} = wabbits2(Months-1, MonthsEachRabbitWillLive, PropListN2),
	Value = ValueN1 + ValueN2 - 1,
	PropListRet = [{Months, Value} | PropListN1],
	{Value, PropListRet};
wabbits3(Months, MonthsEachRabbitWillLive, false, PropList) ->
	{ValueM , PropListM} = wabbits2(Months - MonthsEachRabbitWillLive-1, MonthsEachRabbitWillLive, PropList),
	{ValueN2 , PropListN2} = wabbits2(Months-2, MonthsEachRabbitWillLive, PropListM),
	{ValueN1, PropListN1} = wabbits2(Months-1, MonthsEachRabbitWillLive, PropListN2),
	Value = ValueN1 + ValueN2 - ValueM,
	PropListRet = [{Months, Value} | PropListN1],
	{Value, PropListRet}.

%97, 16 = 82225911288903110704
%83, 18 = 98682772786101696