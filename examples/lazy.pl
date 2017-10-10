:- module(cclazy, [lazy/2, run_memo/1, run_memo//1, flip//1, flip/1, two_flip/2]).
% ----------- thread local memoisation for random world semantics ----------
:- use_module(library(delimcc), [p_shift/2]).
:- use_module(library(ccstate), [run_state_handler//3]).
:- use_module(library(rbutils)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/macros)).

:- meta_predicate lazy(1,-).
lazy(P,X) :- p_shift(memo,lazy(P,X)).

run_memo(Goal) :- rb_empty(E), run_state_handler(memo,handle_memo,Goal,E,_).

run_memo(Goal) --> run_state_handler(memo,handle_memo,Goal).
handle_memo(lazy(P,X)) --> rb_get(P,X) -> []; run_memo(call(P,X)), rb_add(P,X).

flip +-> [t,f].
flip(X) :- lazy(:=(flip),X).
two_flip(X,Y) :- flip(X), flip(Y).
