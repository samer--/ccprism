:- module(ccp_macros, [op(1150,fx,cctable), op(1200,xfx,+->)]).
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
user:term_expansion((:- cctable(Specs)), Clauses) :-
   foldl_clist(expand_cctab, Specs, Clauses, []).

foldl_clist(P,(A,B)) --> !, call(P,A), foldl_clist(P,B).
foldl_clist(P,A) --> call(P,A).

expand_cctab(Name//Arity) --> !,
   {A2 is Arity+2},
   expand_cctab(Name/A2).
expand_cctab(Name/Arity) -->
   { functor(Head, Name, Arity), head_worker(Head, Worker)},
   [ (:- discontiguous('$cctabled'/1))
   , '$cctabled'(Head)
   , (Head :- cctabled(Head,Worker))
   ].

prolog:rename_predicate(M:Head, M:Worker) :-
   '$flushed_predicate'(M:'$cctabled'(_)),
   call(M:'$cctabled'(Head)), !,
   head_worker(Head, Worker).

head_worker(Head, Worker) :-
   Head   =.. [H|As], atom_concat(H,'#',W),
   Worker =.. [W|As].

/* Switches are represented by callable terms which must have type
   =|switch(A) == pred(-switch(A), -list(A), +list(A))|=, where =|A|= is the
   type of the switch's possible values. The predicate must return a canonical
   representation of the switch predicate (eg with a its source module specifier),
   along with a difference list representation of the switch domain.

   This term expansion takes care of the canonical representation part.
*/
:- op(1200,xfx,+->).
user:term_expansion(Lab +-> Body, Clause) :-
   prolog_load_context(module,Module),
   Lab =.. Args,   append(Args, [Module:Lab], Args1),
   Head =.. Args1, dcg_translate_rule(Head --> Body, Clause).
