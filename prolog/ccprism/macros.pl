:- module(ccp_macros, [op(1150,fx,cctable), op(1200,xfx,+->), head_worker/2]).
/** <module> Term expansions to support tabling and switches

This module implements a shallow program transformation to support
tabling. Predicates decalared `cctabled` are renamed (by a appending
a '#' to their given name) and the original predicate name defined as
a metacall of the renamed predicate via cctable/2, which is assumed
to be available in the module where the tabled predicate is defined.
*/

:- op(1150,fx,cctable).

%% cctable(PredSpecfiers) is det
%  Declare predicates in PredSpecifiers (a comma separated list of Name/Arity
%  predicate specifier) as tabled.

foldl_clist(P,(A,B)) --> !, call(P,A), foldl_clist(P,B).
foldl_clist(P,A) --> call(P,A).

head_worker(Head, Worker) :-
   Head   =.. [H|As], atom_concat(H,'#',W),
   Worker =.. [W|As].

decl(F//A) --> !, {A2 is A+2}, decl(F/A2).
decl(F/A) -->
   { functor(Head, F, A), head_worker(Head, Work)},
   [ (:- discontiguous('$cctabled'/2))
   , '$cctabled'(F, A)
   , (Head :- cctabled(Head,Work))
   ].

rename_tabled(Extra, Head, Work) :-
   prolog_load_context(module, M),
   current_predicate(M:'$cctabled'/2),
   functor(Head, F, A), A2 is A+Extra,
   M:'$cctabled'(F,A2),
   head_worker(Head, Work).

expand((:- cctable(Specs)), Clauses) :- !, foldl_clist(decl, Specs, Clauses, []).
expand((Head, P --> Body), (Head2, P --> Body)) :- !, rename_tabled(2, Head, Head2).
expand((Head --> Body),    (Head2 --> Body))    :- !, rename_tabled(2, Head, Head2).
expand((Head :- Body),     (Head2 :- Body))     :- !, rename_tabled(0, Head, Head2).
expand(Head,               Head2)               :- rename_tabled(0, Head, Head2).

system:term_expansion(T1, T2) :- expand(T1, T2).

/* Switches are represented by callable terms which must have type
   =|switch(A) == pred(-switch(A), -list(A), +list(A))|=, where =|A|= is the
   type of the switch's possible values. The predicate must return a canonical
   representation of the switch predicate (eg with a its source module specifier),
   along with a difference list representation of the switch domain.

   This term expansion takes care of the canonical representation part.
*/
:- op(1200,xfx,+->).
system:term_expansion(Lab +-> Body, (Head --> Body)) :-
   prolog_load_context(module,Module),
   Lab =.. Args,   append(Args, [Module:Lab], Args1),
   Head =.. Args1.
