:- start_doc.
:- use_module(library(plrand)).
:- use_module(library(listutils), [zip/3, drop/3]).
:- use_module(library(ccprism/handlers)).
:- use_module(library(ccprism/learn)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/graph)).
:- use_module(library(ccprism)).
:- use_module(library(autodiff2)).
:- use_module(library(julia)).
:- use_module(juliaflow).
:- use_module(dice).
:- use_module(tools).

:- initialization(init, program).

init :-
   persistent_history,
   confirm_on_halt,
   init_rnd_state(S), nb_setval(rs,S),
   init_julia.

init_julia :-
   !using('Plots'), !gr(),
   !default(show=false, size= #(300, 150)).


with_die_sampler(Goal) -->
   {make_lookup_sampler([(dice:die)-[0.2,0.4,0.1,0.3]], S)},
	run_sampling(S, Goal).

histogram(Xs, bar(Vals, Counts)) :-
   histof(Xs, Hist), zip(Vals, Counts, Hist).

multitrial(Learner, K, N, Curves) :-
   nmaplist(N,dice(K),Xs),
   histogram(Xs, Hist), !savefig(Hist, "hist.pdf"),
   goal_graph(maplist(dice(K),Xs), G),
   maplist(call(Learner, G), Curves).

learn1(Modifier, Drop, Tol, Meth, G, H) :-
   graph_params(random, G, P0),
   converge(abs(Tol), call(Modifier, learn(ml, io(Meth), G)), HFull, P0, _P1),
   drop(Drop, HFull, H).

add_plot(Drop, Y, P1, P2) :-
   length(Y, NumIts),
   numlist(1, NumIts, X),
   P2 ?? 'plot!'(P1, Drop+X, Y).

with_plot(Setup, with_plot(Step)) :- call(Setup, Step).
with_plot(Step, Cost, S1, S2) :-
   call(Step, Cost, S1, S2),
   member((dice:die)-Probs, S2),
   format(string(Title), "~1f", [Cost]),
   !gui(1, bar(Probs, title=Title, size= #(160, 160), ylim= #(0,0.6))).

run(Mod,Drop,Tol,K,N,T) :-
   length(Curves,T),
   with_brs(rs, with_die_sampler(multitrial(learn1(Mod, Drop, Tol, log), K, N, Curves))),
   format(string(Title), "dice: K=~w, N=~w, tol=~g", [K,N,Tol]),
   P0 = plot(grid=true, title=Title, xlabel="iteration", ylabel="log likelihood"),
   foldl(add_plot(Drop), Curves, P0, PP),
   !savefig(PP, "curves.pdf").

% print_fig(pdf, plot_histogram("dice(3,_)",Hist), 'hist.pdf', [size(16,10)]),
% print_fig(pdf, r(plot(History)), 'cost.pdf', [size(16,10)]).
thingy(inside, G, P0, [TopVal], TopVal) :-
   semiring_graph_fold(r(autodiff2:log,autodiff2:lse,autodiff2:add_to_wsum,cons), G, P0, IG),
   expand_wsums, top_value(IG, TopVal).

thingy(io(ISc), G, P0, [LogProb|Outs], LogProb-Eta) :-
   graph_counts(io(ISc), lin, G, P0, Eta, LogProb),
   params_variables(Eta, Outs).

mode_graph_body(Mode, G, P0, Result, Body) :-
   time(thingy(Mode, G, P0, Outs, Result)),
   params_variables(P0, Ins),
   gather_ops(Ops),
   length(Ops, NumOps),
   format('Compiling ~d ops...\n', [NumOps]),
   time(topsort(Ins, Outs, Ops, SortedOps)),
   ops_body(SortedOps, Body).

params_variables(Params, Ins) :- foldl(probs, Params, [], Ins).
probs(_-Probs) --> append(Probs).

:- meta_predicate with_compiled_lambda(+,-,0).
with_compiled_lambda(Lambda, Pred, Goal) :-
   flag(dpred, I, I+1), atom_number(Pred, I),
   lambda_clause(Lambda, Pred, Arity, Clause),
   setup_call_cleanup(assert(Clause), Goal, abolish(Pred/Arity)).

:- meta_predicate with_compiled_lambda2(+,-,0).
with_compiled_lambda2(\Lambda, dpred(I), Goal) :-
   flag(dpred, I, I+1),
   once(lambda_clause(\I^Lambda, dpred, _, Clause)),
   setup_call_cleanup(assert(Clause), Goal, retract(Clause)).

lambda_clause(\X^Y^Z^Body, Pred, 3, Head :- Body) :- Head =.. [Pred, X, Y, Z].
lambda_clause(\X^Y^Body, Pred, 2, Head :- Body) :- Head =.. [Pred, X, Y].

speed_test(Mode,K,N,M) :-
   writeln('Timings are: search, build_chr, topsort, total_setup, iterations'),
   with_brs(rs, with_die_sampler(nmaplist(N, dice(K), Xs))),
   goal_graph(maplist(dice(K),Xs), G),
   graph_params(uniform, G, P0),
   time(mode_graph_body(Mode, G, P, Top, Body)),
   time(with_compiled_lambda(\P^Top^Body, Pred, nmaplist(M, call(Pred, P0), _Vals))).
