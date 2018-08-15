:- module(tools, [rep/2, iota//1, nathist/3, histof/2, distof/2]).

:- use_module(library(data/pair), [fsnd/3]).
:- use_module(library(math), [divby/3]).

%% iota(+N:natural, L1:list(natural), L2:list(natural)) is det.
%  Difference list version of numlist, useful for switch domains.
iota(0,L,L) :- !.
iota(N,L3,L1) :- succ(M,N), iota(M,L3,[N|L1]).

rep(0,_) :- !.
rep(N,G) :- N1 is N-1, call(G), rep(N1,G).

histof(Xs,Hist) :- setof(X-N, aggregate(count,member(X,Xs),N), Hist).
distof(Xs,Dist) :-
   histof(Xs,Hist),
   length(Xs,N),
   maplist(fsnd(divby(N)), Hist, Dist).

nathist(Dom,Ns,H) :-
	setof(N-C, aggregate(count, member(N,Ns), C), NCs),
	maplist(lup(NCs), Dom, H).
lup(NCs,N,C) :- member(N-C,NCs) -> true; C=0.
