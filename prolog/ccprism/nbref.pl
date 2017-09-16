:- module(nbref, [with_nbref/2, nbref_new/3]).

:- meta_predicate with_nbref(-,0).
with_nbref(E, Goal) :- setup_call_cleanup(setup(E), Goal, cleanup(E)).

setup(E) :- gensym(nbref,ID), atom_concat(ID,'.',E), nb_setval(E, 0).
cleanup(E) :- nb_getval(E, N), nb_delete(E), forall(between(1,N,I), delete(E,I)).
delete(E,I) :- atomic_concat(E,I,Ref), nb_delete(Ref).

nbref_new(E, Value, Ref) :-
   nb_getval(E, I), J is I+1, atomic_concat(E, J, Ref),
   nb_setval(Ref, Value), nb_setval(E,J).
