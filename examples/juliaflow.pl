:- module(juliaflow, [topsort/4, ops_body/2]).

:- use_module(library(julia)).
:- use_module(library(rbutils)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(insist)).
:- use_module(library(math), [divby/3]).

topsort(Ins, Outs, Ops, SortedOps) :-
   rb_empty(E),
   foldl(back_links, Ops, E, BS),
   traverse(BS, Ins, Outs, SortedOps-E, []-_).

back_links(Edge) --> {Edge=op(_,_,Outs)}, foldl(back_link(Edge), Outs).
back_link(Edge, Out) --> rb_add(Out, Edge).
traverse(BS, Ins, Outs) --> \> foldl(insert, Ins), foldl(eval(BS), Outs).
insert(X) --> rb_add(X,t).

eval(BS, Var) -->
   (  ({nonvar(Var)}; \> rb_get(Var, _)) -> []
   ;  {rb_lookup(Var, Edge, BS), Edge=op(_,Ins,Outs)},
      foldl(eval(BS), Ins),
      [Edge] <\> foldl(insert, Outs)
   ).

ops_body(Ops, Body) :- foldl(op_goal, Ops, Body, true).
op_goal(op(OpCode, Ins, Outs), (X,Y), Y) :- insist(op_goal(OpCode, Ins, Outs, X)).

:- multifile op_goal/4.
op_goal(add, [X,Y], [Z], Z is X + Y).
op_goal(sub, [X,Y], [Z], Z is X - Y).
op_goal(mul, [X,Y], [Z], Z is X * Y).
op_goal(div, [X,Y], [Z], Z is X / Y).
op_goal(pow, [X,Y], [Z], Z is Y**X).
op_goal(exp, [X], [Z], Z is exp(X)).
op_goal(log, [X], [Z], Z is log(X)).
op_goal(chi, [X,Y,Z], [I], (X>Y -> I=Z; X<Y -> I=0.0; I is Z/2.0)).
op_goal(sum_list,   Xs, [Z], sum_list(Xs,Z)).
op_goal(max_list,   Xs, [Z], max_list(Xs,Z)).
op_goal(divby_list, [K|Xs], Zs, juliaflow:divby_list(K, Xs, Zs)).

divby_list(K, Xs, Zs) :- maplist(divby(K), Xs, Zs).
