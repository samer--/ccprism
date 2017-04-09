#! /usr/bin/env swipl -O -g init
:- module(ccp_test, []).

:- use_module(library(data/pair)).
:- use_module(library(callutils)).
:- use_module(library(listutils), [take/3]).
:- use_module(library(math)).
:- use_module(library(lambda2)).
:- use_module(library(delimcc), [ccshell/0]).
:- use_module(library(prob/strand)).
:- use_module(library(ccprism/machines)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/handlers)).
:- use_module(library(ccprism/graph)).
:- use_module(library(ccprism/switches)).
:- use_module(library(ccprism/mcmc)).
:- use_module(library(ccprism/display)).
:- use_module(library(ccprism), [goal_graph/2]).
:- use_module(models).

:- set_prolog_flag(back_quotes, symbol_char).

:- meta_predicate samp(0).
samp(G) :- strand(run_sampling(uniform_sampler,G)).
unfold(N0,M,S) :- succ(N0,N), time(samp(call(take(N)*unfold, M, [_|S]))).

make_dataset(N,XX) :-
   length(XX,N),
   biased_sampler(SS),
   strand(run_sampling(SS, maplist(phrase(s), XX))).

assert_new_data(I,N) :- make_dataset(N,X), assert(dataset(I,X)).

init :- maplist(assert_new_data,[1,2,3],[10,20,30]).

user:portray(X) :- float(X), !, format('~5g',[X]).

dice_gibbs(AA,Stride,Spec>F,M) :-
   goal_graph(maplist(two_dice,[4,4,4]), G), 
   graph_params((math:mul(AA))*uniform,G,P0), 
	call(call(Spec,G,P0,P0) >> drop(500) >> subsample(Stride) >> mapper(snd*nth1(1)*F), M).

gibbs_samples(AA,Spec,K,NumSamples,S) :- unfold(NumSamples, dice_gibbs(AA,K,Spec), S).

seq_dist(S,CC) :-
	length(S,NumSamples),
   setof(X-N, aggregate(count,member(X,S),N), HH), 
   maplist(fsnd(divby(NumSamples-1)),HH,CC). 

counts(Counts) :- setof( Xs, expl_stats(Xs), Counts).

counts_multiplicities(HH) :-
	setof(S-N, aggregate(count, E^(expl(E), nathist([1,2,3],E,S)), N), HH).

expl(E) :- length(E,3), maplist(between(1,3),E).
expl_stats(Xs) :- length(Xs,3), maplist(between(0,3),Xs), sumlist(Xs,3).

exact_probs(AA, Dist) :-
   listutils:rep(3,A,Alphas), A is AA/3,
   counts_multiplicities(HH), 
   maplist(pair, CC, NN, HH),
   maplist(pair, CC, Ps, Dist),
   maplist(exp*mul(2)*marg_log_prob(Alphas),CC,Ws), 
   maplist(mul,NN,Ws,Ws1),
   stoch(Ws1,Ps,_).

uncurry(P,X-Y,Z) :- call(P,X,Y,Z).

histof(Xs,Hist) :- setof(X-N, aggregate(count,member(X,Xs),N), Hist).
distof(Xs,Dist) :-
   histof(Xs,Hist),
   length(Xs,N),
   maplist(fsnd(divby(N)), Hist, Dist).

nathist(Dom,Ns,H) :- 
	setof(N-C, aggregate(count, member(N,Ns), C), NCs),
	maplist(lup(NCs), Dom, H).
lup(NCs,N,C) :- member(N-C,NCs) -> true; C=0.

ind(Xs,X,Is) :- maplist(eq(X),Xs,Is).
eq(X,Y,I) :- X=Y -> I=1; I=0.

vec_divby(K,X,Y) :- maplist(divby(K),X,Y).

test_mcmc(NumSamples, Sub, Spec, S) :-
	counts(CC), 
   member(Spec>F, [gibbs_posterior_machine(counts)>(=), mc_machine(mh)>mcs_counts, mc_machine(gibbs2)>mcs_counts, mc_machine(gibbs)>mcs_counts]),
	unfold(NumSamples, dice_gibbs(2,Sub,Spec>F) 
                      >> mapper(ind(CC)) 
                      >> mean(maplist(=(0)), maplist(add), vec_divby) 
                      >> drop(2000),
          S).

:- module(ccp_test).
% vim: ft=prolog
