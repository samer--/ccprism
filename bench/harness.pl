:- use_module(library(plrand)).
:- use_module(library(listutils), [drop/3]).
:- use_module(library(ccprism/handlers)).
:- use_module(library(ccprism/learn)).
:- use_module(library(ccprism)).
:- use_module(library(julia)).
:- use_module(hmm).
:- use_module(dice).

:- initialization(init, program).

:- meta_predicate samp(0), samp(4,0), samp(0,+,-).
samp(G) :- samp(uniform_sampler,G).
samp(S,G) :- with_brs(rs, run_sampling(S,G)).
samp(G) --> run_sampling(uniform_sampler,G).

sample_hmm(N, X) :-
   length(X, N),
   samp(hmm(x, X)).

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

init :-
   % set_prolog_flag(toplevel_mode, recursive),
   init_rnd_state(S), nb_setval(rs,S),
   init_julia.

init_julia :-
   !using('Plots'),
   !unicodeplots(),
   !default(show=false, size= #(900,400)).

add_plot(X, P1, P2) :- P2 ?? 'plot!'(P1, X).

empty_plot(plot(xlabel="iteration", ylabel="log likelihood")).
learn_history(Meth, K, N, H) --> sample_and_learn_dice(Meth, K, N, [_|H], _).

with_die_sampler(Goal) -->
   {make_lookup_sampler([(dice:die)-[0.2,0.4,0.1,0.3]], S)},
	run_sampling(S, Goal).

multitrial(Learner, K, N, Curves) :-
   length(Xs,N), maplist(dice(K),Xs),
   goal_graph(maplist(dice(K),Xs), G),
   maplist(call(Learner, G), Curves).

learn1(Modifier, Drop, Meth, G, H) :-
   graph_params(random, G, P0),
   converge(abs(1e-5), call(Modifier, learn(ml, io(Meth), G)), HFull, P0, _P1),
   drop(Drop, HFull, H).

multiplot(Curves, P1) :- empty_plot(P0), foldl(add_plot, Curves, P0, P1).

with_plot(Setup, with_plot(Step)) :- call(Setup, Step).
with_plot(Step, Cost, S1, S2) :-
   call(Step, Cost, S1, S2),
   member((dice:die)-Probs, S2),
   format(string(Title), "~1f", [Cost]),
   !gui(1, bar(Probs, title=Title, size= #(160, 160), ylim= #(0,0.6))).
