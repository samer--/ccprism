:- module(ccp_kbest, [graph_nviterbi/4]).

/** <module> Lazy k-best parsing
   This module provides a semiring for generating parse trees lazily in best-first
   order, based on the algorithm of Huang and Chiang [1]. Unlike their method
   however, this needs no preassigned limit on the number of parses to produce.

   [1] Liang Huang and David Chiang. Better k-best parsing. 
       In Proceedings of the Ninth International Workshop on Parsing Technology, pages 53–64.
       Association for Computational Linguistics, 2005.
*/
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lazy), [lazy_maplist/3, lazy_unfold_finite/4]).
:- use_module(lazymath, [surp/2, lazy/4]).
:- use_module(graph, [semiring_graph_fold/4, top_value/2]).

%% graph_nviterbi(+G:graph, +P:sw_params, -T:expl_tree, -LP:number) is nondet.
%
%  Find the most probable parse tree, and then find progressively less probable
%  parse trees on backtracking.
graph_nviterbi(Graph, Params, Tree, LP) :-
   semiring_graph_fold(kbest, Graph, Params, VGraph), 
   top_value(VGraph, Expls),
   member(LP-Tree,Expls).

ccp_graph:sr_inj(kbest,   F, P, [Q-F]) :- surp(P,Q).
ccp_graph:sr_proj(kbest,  G, X, X, Y)  :- freeze(Y,lazy_maplist(k_tag(G),X,Y)).
ccp_graph:sr_plus(kbest,  X) --> lazy(k_min,X).
ccp_graph:sr_times(kbest, X) --> lazy(k_mul,X).
ccp_graph:sr_zero(kbest,  []).
ccp_graph:sr_unit(kbest,  [0-[]]).

k_tag(G,L-X,L-(G-X)). % tag explanaiton with head goal
k_min([],Y,Y) :- !.
k_min(X,[],X) :- !.
k_min([X|Xs],[Y|Ys],[Z|Zs]) :-
   (  LX-_=X, LY-_=Y, LX =< LY
   -> Z=X, freeze(Zs, k_min(Xs,[Y|Ys],Zs))
   ;  Z=Y, freeze(Zs, k_min([X|Xs],Ys,Zs))
   ).

k_mul(X,Y,Z) :-
   empty_set(EmptyS), empty_heap(EmptyQ),
   k_queue(0^X-0^Y, EmptyS-EmptyQ, TQ1),
   lazy_unfold_finite(k_next, Z, TQ1, _).

k_next(L-[XF|YFs]) -->
   \> pq_get(L,P),
   {P=I^[X0|X]-J^[Y0|Y], _-XF=X0, _-YFs=Y0}, 
   {succ(J,J1)}, k_queue(I^X-J1^[Y0|Y]),
   {succ(I,I1)}, k_queue(I1^[X0|X]-J^Y).

k_queue(P) --> {P=I^X-J^Y}, \< add_to_set(I-J), {k_cost(X,Y,L)} -> \> pq_add(L,P); [].
k_cost([X0-_|_],[Y0-_|_], L) :- L is X0+Y0.

pq_add(L,P,H1,H2) :- add_to_heap(H1,L,P,H2).
pq_get(L,P,H1,H2) :- get_from_heap(H1,L,P,H2).
add_to_set(X,S1,[X|S1]) :- \+memberchk(X,S1).
empty_set([]).
