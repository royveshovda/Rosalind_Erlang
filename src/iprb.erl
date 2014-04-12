-module(iprb).
-export([prob/3]).

prob(K, M, N) ->
	Sum = K + M + N,
    Prob = (N * (N - 1) + N * M + M * (M - 1) / 4.0),
    (1 - 1 / Sum / (Sum - 1) * Prob).