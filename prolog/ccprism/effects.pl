:- module(ccp_effects, [ ccstore/2, ccstored/1, cctabled/2, uniform/2, dist/2, dist/3, (:=)/2, sample/2 ]).

/** <module> Computational effects for supporting probabilistic models */

:- use_module(library(listutils),   [zip/3]).
:- use_module(library(delimcc),     [p_shift/2]).

:- meta_predicate :=(3,-), ccstore(:,0), ccstored(:), cctabled(:,0), sample(3,-).

%% dist(+Dist:list(pair(prob,A)), -X:A) is nondet.
%% dist(+Ps:list(prob), +Xs:list(A), -X:A) is nondet.
%  Probabilistic choice from given distribtion, which can be supplied as a
%  list of pairs (dist/2) or two lists (dist/3).
dist(Dist,X)  :- zip(Ps,Xs,Dist), p_shift(prob,dist(Ps,Xs,X)).
dist(Ps,Xs,X) :- p_shift(prob,dist(Ps,Xs,X)).

%% uniform(+Xs:list(A), -A) is nondet.
%  Probabilistic choice from uniform distribution over list of values.
uniform(Xs,X) :- p_shift(prob,uniform(Xs,X)).

%% sample(+P:pred(-A,+strand,-strand), -A) is det.
%  Do arbitrary random sampling using capabilities of prob/strand.pl.
%  NB. only works during sampling execution, not explanation search.
sample(P,X)   :- p_shift(prob,sample(P,X)). 

%% ((+SW:switch(A)) := (-X:A)) is nondet.
%  Probabilistic choice from switch. A switch SW is a callable term of type:
%  ==
%  switch(A) == pred(-switch(A), -X1:list(A), X2:list(A))
%  ==
%  such that =|call(SW,ID,X1,X2)|= unifies ID with a canonical callable form of the
%  switch and X1-X2 with a difference list of the switch's possible value.
SW := X       :- p_shift(prob,sw(SW,X)).

%% cctabled(:Head, +Work:callable) is nondet.
%  Execute Work using tabled execution and storing the result under Head.
%  See ccprism/macros.pl for an automatic program transformation to
%  manage tabled predicates.
cctabled(Head,Work) :- p_shift(tab, tcall(Head,Work,Inj)), call(Inj).

%% ccstore(:Head, +Work:callable) is det.
%  Stores a pre-computed 'pseudo-goal' in the tables, similar to Prolog's
%  assert. The result of calling =|once(Work)|= is stored under the variant form Head.
%  Thus, Work is expected to bind the variables in Head which represent the
%  result of the tabled computation. Stored results should be retrieved using
%  ccstored/1, not cctabled/2.
ccstore(Head,Work)  :- copy_term(Head-Work,H-W), p_shift(tab, tab(H,once(W),_)).

%% ccstored(:Head) is det.
%  Look up tabled fact previously stored using ccstore/2. Throws an exception
%  if no such fact is found. 
ccstored(Head)      :- p_shift(tab, tab(Head,throw(not_stored(Head)),_)).


