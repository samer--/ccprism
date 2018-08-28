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
:- use_module(library(lazy), [lazy_maplist/3, lazy_unfold_finite/4, lazy/4]).
:- use_module(graph, [graph_fold/4, top_value/2]).

%% graph_nviterbi(+G:graph, +P:sw_params, -T:expl_tree, -LP:number) is nondet.
%
%  Find the most probable parse tree, and then find progressively less probable
%  parse trees on backtracking.
graph_nviterbi(Graph, Params, Tree, LP) :-
   graph_fold(kbest, Params, Graph, VGraph),
   top_value(VGraph, Expls),
   member(LP-Tree,Expls).

ccp_graph:sr_inj(kbest,   P, F, [Q-F]) :- when(ground(P), Q is -log(P)).
ccp_graph:sr_proj(kbest,  G, X, Y, X)  :- freeze(Y, lazy_maplist(k_tag(G),X,Y)).
ccp_graph:sr_plus(kbest,  X) --> lazy(k_min,X).
ccp_graph:sr_times(kbest, X) --> lazy(k_mul,X).
ccp_graph:sr_zero(kbest,  []).
ccp_graph:sr_unit(kbest,  [0.0-[]]).

k_tag(G,L-X,L-(G-X)). % tag explanation with head goal

k_min([],Ys,Ys) :- !.
k_min(Xs,[],Xs) :- !.
k_min([X|Xs],[Y|Ys],[Z|Zs]) :-
   (  LX-_=X, LY-_=Y, LX =< LY
   -> Z=X, freeze(Zs, k_min(Xs,[Y|Ys],Zs))
   ;  Z=Y, freeze(Zs, k_min([X|Xs],Ys,Zs))
   ).

k_mul(Xs,Ys,Zs) :-
   empty_set(EmptyS), empty_heap(EmptyQ),
   enqueue(pos(0-0,Xs,Ys), EmptyS-EmptyQ, TQ1),
   lazy_unfold_finite(k_next, Zs, TQ1, _).

k_next(L-[XF|YFs]) -->
   \> pq_get(L,pos(I-J,[X0|Xs],[Y0|Ys])),
   {_-XF=X0, _-YFs=Y0, succ(I,I1), succ(J,J1)},
   enqueue(pos(I1-J,Xs,[Y0|Ys])),
   enqueue(pos(I-J1,[X0|Xs],Ys)).

enqueue(P) --> new_position_cost(P,L) -> \> pq_add(L,P); [].
new_position_cost(pos(IJ,[X0-_|_],[Y0-_|_]),L) --> \< add_to_set(IJ), {L is X0+Y0}.

pq_add(L,P,H1,H2) :- add_to_heap(H1,L,P,H2).
pq_get(L,P,H1,H2) :- get_from_heap(H1,L,P,H2).
add_to_set(X,S1,[X|S1]) :- \+memberchk(X,S1).
empty_set([]).
% alternative, better for high k
% add_to_set(X,S1,S2) :- rb_insert_new(S1,X,t,S2).
% empty_set(S) :- rb_empty(S).
