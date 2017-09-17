:- module(ccp_handlers, [ goal_expls_tables/3, run_incr/1, run_tab/3, run_sampling//2, run_prob//2
                        , expl//1, uniform_sampler//2, make_lookup_sampler/2, fallback_sampler//4
                        ]).

/** <module> Effect handlers for probabilistic programming

   This module provides tabled explanation search and sampling as computational
   effects using delimited control.

   @tbd
   - Goal subsumtion in table lookup
   - Lazy explanation search (see ccbeam in cclab)
*/
:- use_module(library(typedef)).
:- use_module(library(lambdaki)).
:- use_module(library(prob/tagged), [discrete//3, uniform//2]).
:- use_module(library(prob/strand), [pure//2]).
:- use_module(library(delimcc),     [p_reset/3, p_shift/2]).
:- use_module(library(rbutils),     [rb_app_or_new/5, rb_in/3]).
:- use_module(library(ccnbref),     [run_nb_ref/1, nbref_new/2, nbref_add_with/3]).

:- type table  ---> tab(rbtree(values, list(list(factor))), list(cont)).
:- type factor ---> module:head ; @number ; sw(A):=A.
:- type cont   == pred(+values, -values).
:- type values == list(ground).

:- meta_predicate run_prob(3,0,?,?).
% run_prob(Handler,Goal) --> run_state_handler(prob, Handler, Goal).
run_prob(Handler,Goal) --> {p_reset(prob, Goal, Status)}, cont_prob(Status,Handler).
cont_prob(susp(Req,Cont),H) --> call(H,Req), run_prob(H,Cont).
cont_prob(done,_) --> [].

% ------------- handlers for sampling without tabling ------------------
sample(P,sw(SW,X))      --> !, call(P,SW,X).
sample(_,dist(Ps,Xs,X)) --> !, pure(discrete(Xs,Ps),X).
sample(_,uniform(Xs,X)) --> !, pure(uniform(Xs),X).
sample(_,sample(P,X))   --> call(P,X).

run_notab(Goal) :- p_reset(tab, Goal, Status), cont_notab(Status).
cont_notab(susp(tcall(_,Head,Head), Cont)) :- run_notab(Cont).
cont_notab(done).

:- meta_predicate run_sampling(4,0,+,-).
run_sampling(Sampler,Goal,S1,S2) :-
   run_notab(run_prob(sample(Sampler),Goal,S1,S2)).

uniform_sampler(SW,X) --> {call(SW,_,Xs,[])}, pure(uniform(Xs),X).
lookup_sampler(Map,SW,X) --> {call(SW,ID,Xs,[]), rb_lookup(ID,Ps,Map)}, pure(discrete(Xs,Ps),X).
make_lookup_sampler(Params,ccp_handlers:lookup_sampler(Map)) :- list_to_rbtree(Params, Map).
fallback_sampler(S1, S2, SW,X) --> call(S1,SW,X) -> []; call(S2,SW,X).

% -------- handlers for tabled explanation graph building -----------
% goal_expls_tables(+Goal,-TopExpls:list(list(factor)), -Tables) is det.
%
% Runs goal with tabling and explanation building effects to find all explanations 
% for the top goal, and the tables for everything else, from which the rest of
% an explanation graph can be built.
:- meta_predicate goal_expls_tables(0,-,-).
goal_expls_tables(G,Es,Tabs) :- run_nb_ref(nb_goal_expls_tables(G,Es,Tabs)).
nb_goal_expls_tables(G,Es,Tabs) :-
   trie_new(Trie), % could pass Trie in and out to allow reuse...
   run_tab(findall(E,run_prob(expl,G,E,[]),Es), Trie, Es),
   trie_tables(Trie, Tabs).

%% run_incr(+Goal) is nondet.
%  Runs goal in explanation search mode but produces solutions incrementally, 
%  discarding top explanation and not retrieving tables.
:- meta_predicate run_incr(0).
run_incr(Goal) :-
   trie_new(Trie), term_variables(Goal, Ans),
   run_nb_ref(run_tab(run_prob(expl, Goal, _, []), Trie, Ans)).

expl(tab(G))     --> {term_to_ground(G,F)}, [F].
expl(sw(SW,X))   --> {call(SW,ID,Xs,[]), member(X,Xs)}, [ID:=X].
expl(dist(Ps,Xs,X)) --> {member2(P,X,Ps,Xs)}, [@P].
expl(uniform(Xs,X)) --> {length(Xs,N), P is 1/N, member(X,Xs)}, [@P].

:- meta_predicate run_tab(0,+,?).
run_tab(Goal, Trie, Ans) :- p_reset(tab, Goal, Status), cont_tab(Status, Trie, Ans).

cont_tab(done, _, _).
cont_tab(susp(tcall(TableAs,Work,ccp_handlers:p_shift(prob,tab(TableAs))), Cont), Trie, Ans) :-
   term_variables(TableAs, Y), K = k(Y,Ans,Cont),
   (  trie_lookup(Trie, TableAs, tab(SolnTrie,KsRef))
   -> nbref_add_with(KsRef, post_prepend, K), 
      trie_gen(SolnTrie, Y, _), 
      run_tab(Cont, Trie, Ans)
   ;  nbref_new([K], KsRef), trie_new(SolnTrie),
      trie_insert(Trie, TableAs, tab(SolnTrie,KsRef)),
      run_tab(producer(\Y^Work, KsRef, SolnTrie, Ans), Trie, Ans)
   ).

producer(Generate, KsRef, SolnTrie, Ans) :-
   run_prob(expl, call(Generate, Y), E, []),
   (  trie_lookup(SolnTrie,Y,EsRef)
   -> nbref_add_with(EsRef, prepend, E), fail
   ;  nbref_new([E], EsRef), trie_insert(SolnTrie,Y,EsRef),
      nb_getval(KsRef, Ks0), copy_term(Ks0,Ks),
      member(k(Y,Ans,C), Ks), call(C)
   ).

trie_tables(Trie, TList) :- 
   findall(Table, trie_table(Trie,Table), TList).

trie_table(Trie, Head-Solns) :-
   trie_gen(Trie, Head, tab(SolnTrie,_)),
   findall(Soln, soln_trie_solns(SolnTrie,Soln), Solns).

soln_trie_solns(SolnTrie,Y-Es) :- 
   trie_gen(SolnTrie, Y, EsRef), nb_getval(EsRef, Es).

term_to_ground(T1, T2) :- copy_term_nat(T1,T2), numbervars(T2,0,_).
member2(X,Y,[X|_],[Y|_]).
member2(X,Y,[_|XX],[_|YY]) :- member2(X,Y,XX,YY).
post_prepend(X1,[X0|Xs],[X0,X1|Xs]).
prepend(X1,Xs,[X1|Xs]).

