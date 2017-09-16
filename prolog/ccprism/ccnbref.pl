:- module(ccnbref, [run_nb_ref/1, nbref_new/2, nbref_app/2, nbref_get/2]).

:- use_module(library(delimcc), [p_reset/3, p_shift/2]).
:- use_module(nbref, [with_nbref/2, nbref_new/3]).

:- meta_predicate nbref_app(+,2).
nbref_new(X, Ref) :- p_shift(nbref, X-Ref).
nbref_app(Ref, P) :- nb_getval(Ref, X1), call(P,X1,X2), nb_setval(Ref, X2).
nbref_get(Ref, X) :- nb_getval(Ref, X).

:- meta_predicate run_nb_ref(0).
run_nb_ref(Goal) :- with_nbref(E, run(Goal, E)).

run(Goal, E) :- p_reset(nbref, Goal, Status), cont(Status, E).

cont(susp(X-Ref,Cont), E) :- nbref_new(E, X, Ref), run(Cont, E).
cont(done, _).
