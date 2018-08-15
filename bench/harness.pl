:- use_module(library(plrand)).
:- use_module(library(ccprism/handlers)).
:- use_module(library(ccprism/learn)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism)).
:- use_module('../examples/dice').
:- use_module('../examples/hmm').

:- initialization(init, program).

sample_hmm(N, X) :-
   length(X, N),
   with_brs(rs, run_sampling(uniform_sampler, hmm(x, X))).

seq_to_hmm_goal(hmm, X, hmm(x,X)).
seq_to_hmm_goal(db, X, hmm_db(test,x,0)) :- assert_seq(test, X).

sample_and_learn_hmm(Model,Meth, N, H, P1) :-
   sample_hmm(N, X),
   seq_to_hmm_goal(Model, X, Goal),
   goal_graph(Goal, G),
   graph_params(uniform, G, P0),
   converge(abs(1e-4), learn(ml,io(Meth),G), H, P0, P1).

sample_and_learn_dice(Meth, K, N, H, P1, R1, R2) :-
   length(Xs,N),
   make_lookup_sampler([(dice:die)-[0.2,0.4,0.3,0.1]], S),
	run_sampling(S, maplist(dice(K),Xs), R1 ,R2),
   goal_graph(maplist(dice(K),Xs), G),
   graph_params(uniform, G, P0),
   time(converge(abs(1e-5), learn(ml, io(Meth), G), H, P0, P1)).

init :- init_rnd_state(S), nb_setval(rs,S).
