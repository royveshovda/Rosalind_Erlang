-module(lia).
-export([test/0]).


test() ->
	problem(2,1).


problem(K, N) ->
	problem(K,N,0).

problem(_K,0,Sum) ->
	1-Sum;
problem(K,N,Sum) ->
	NewSum = p(N,K) + Sum,
	problem(K, N-1, NewSum).

% def problem(k, N):
%     """
% Return the answer to the problem.

% The trick is the "at least N" part of the problem description. Asking for
% "at least 1" is actually asking for the sum of all probabilities except
% for the probability that the number of Aa Bb offspring is 0, and we can
% handle that like this:

% 1 - P(X=0)

% Where X is the random variable representing the number of offspring with
% the genotype Aa Bb. Following from this, if we want "at least 2", we should
% actually solve for this:

% 1 - P(X=0) - P(X=1)

% Because we want "at least N", we need a general solution with N:

% 1 - P(X=0) - P(X=1) - ... - P(X=N-1)
% """
%     return 1 - sum([P(n, k) for n in range(N)])



p(N, K) ->
	Binomial = binomial(math:pow(2, K), N),
	Part2 = math:pow(0.25, N),
	Part3 = math:pow(0.75, (math:pow(2,K) - N)),
	Binomial * Part2 * Part3.

% def P(n, k):
%     """
% Return the probability that there are exactly n Aa Bb offspring after
% k generations.

% This can be modeled as a Bernoulli trial, where success is an organism
% has genotype Aa Bb and failure is any other genotype. Doing a Punnett
% square with Aa Bb and Aa Bb shows that probability of any offspring having
% that genotype is 0.25, so we use that as our p value.
% """
%     return binomial(2**k, n) * 0.25**n * 0.75**(2**k - n)



binomial(N, K) when K > (N-K) ->
	K_new = N-K,
	binomial_loop(N, K_new, 1, 1);
binomial(N, K) ->
	binomial_loop(N, K, 1, 1).

binomial_loop(_N, _K, _I, Acc) when _I > (_K+1) ->
	Acc;
binomial_loop(N, K, I, Acc) when I =< (K+1) ->
	New_Acc = (Acc * (N - (K-I)) ) / I,
	binomial_loop(N, K, I+1, New_Acc).

% def binomial(n, k):
%     """Compute n factorial by a direct multiplicative method."""
%     if k > n - k:
%         k = n - k # Use symmetry of Pascal's triangle
%     accum = 1
%     for i in range(1, k + 1):
%         accum *= (n - (k - i))
%         accum /= i
%     return accum
