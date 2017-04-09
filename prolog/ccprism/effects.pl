:- module(ccp_effects, [ ccstore/2, ccstored/1, cctabled/2, uniform/2, dist/2, dist/3, (:=)/2, sample/2 ]).

/** <module> Computational effects for supporting probabilistic models */

:- use_module(library(listutils),   [zip/3]).
:- use_module(library(delimcc),     [p_shift/2]).

:- meta_predicate :=(3,-), ccstore(:,0), ccstored(:), cctabled(:,0), sample(3,-).

dist(Dist,X)  :- zip(Ps,Xs,Dist), p_shift(prob,dist(Ps,Xs,X)).
dist(Ps,Xs,X) :- p_shift(prob,dist(Ps,Xs,X)).
uniform(Xs,X) :- p_shift(prob,uniform(Xs,X)).
sample(P,X)   :- p_shift(prob,sample(P,X)). % currently only for sampling execution!
SW := X       :- p_shift(prob,sw(SW,X)).

ccstored(Head)      :- p_shift(tab, tab(Head,throw(not_stored(Head)),_)).
cctabled(Head,Work) :- p_shift(tab, tab(Head,Work,Inj)), call(Inj).
ccstore(Head,Work)  :- copy_term(Head-Work,H-W), p_shift(tab, tab(H,once(W),_)).

