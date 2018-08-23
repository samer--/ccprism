:- module(ccp_learn, [converge/5, learn/4, learn/5, params_variables/2]).

/** <module> Expectation-maximisation, variational Bayes and deterministic annealing.
*/

:- use_module(library(math),       []).
:- use_module(library(callutils),  [(*)/4, true2/2]).
:- use_module(library(plrand),     [mean_log_dirichlet/2, log_partition_dirichlet/2]).
:- use_module(library(autodiff2),  [esc/3, add/3, mul/3, pow/3, max/3, gather_ops/1]).
:- use_module(library(plflow),     [topsort/4, ops_body/2]).
:- use_module(library(clambda),    [clambda/2, run_lambda_compiler/1]).
:- use_module(lazymath, [map_sum/4]).
:- use_module(graph,    [graph_counts/6]).
:- use_module(switches, [map_sw/3, map_swc/3, map_swc/4, map_sum_sw/3, map_sum_sw/4]).


stoch(Xs,Ys) :- same_length(Xs,Ys), esc(stoch,Xs,Ys).

%! learn(+Method:learn_method, +Stats:stats_method, +ITemp:number, +G:graph, -U:learner) is det.
%! learn(+Method:learn_method, +Stats:stats_method, +G:graph, -U:learner) is det.
%  Get update predicate for several EM-based parameter learning methods. learn/4 invokes
%  learn/5 with ITemp=1.0.
%  ==
%  learn_method ---> ml; map(sw_params); vb(sw_params).
%  stats_method ---> io(scaling); vit.
%  scaling      ---> lin; log.
%  learner == pred(-float, +sw_params, -sw_params).
%  ==
learn(Method, StatsMethod, Graph, Step) :- learn(Method, StatsMethod, 1, Graph, Step).

params_variables(Params, Ins) :- foldl(probs, Params, [], Ins).
probs(_-Probs) --> append(Probs).

log_prob_dir(As, Ps, LP) :- esc(log_prob_dirichlet(As), Ps, [LP]).
log_part_dir(As, LZ) :- esc(log_partition_dirichlet, As, [LZ]).

plflow:op_goal(log_prob_dirichlet(As), Ps, [LP], switches:log_prob_dirichlet(As,Ps,LP)).
plflow:op_goal(log_partition_dirichlet, As, [LZ], plrand:log_partition_dirichlet(As,LZ)).

make_lambda(Obj, P1, P2, Pred) :-
   maplist(params_variables, [P1,P2], [Ins,Outs]),
   gather_ops(Ops), length(Ops, NumOps),
   debug(learn(setup), 'Compiling ~d operations...', [NumOps]),
   call(ops_body * topsort(Ins, [Obj|Outs]), Ops, Body),
   clambda(lambda([Obj,P1,P2], Body), Pred).

learn(ml, Stats, ITemp, Graph, Lambda) :-
   once(graph_counts(Stats, lin, Graph, PP, Eta, LL)),
   map_swc(pow(ITemp), P1, PP),
   map_sw(stoch, Eta, P2),
   make_lambda(LL, P1, P2, Lambda).

% FIXME: probably still a bit broken
learn(map(Prior), Stats, ITemp, Graph, Lambda) :-
   once(graph_counts(Stats, lin, Graph, PP, Eta, LL)),
   map_sum_sw(log_prob_dir, Prior, P1, LP0), % FIXME
   mul(ITemp, LP0, LP),
   map_swc(add, Eta, Prior, Post),
   map_sw(stoch*maplist(max(0.0)*add(-1.0)), Post, P2), % mode
   add(LL,LP,Obj),
   map_swc(pow(ITemp), P1, PP),
   make_lambda(Obj, P1, P2, Lambda).

% FIXME: very broken
learn(vb(Prior), Stats, ITemp, Graph, Lambda) :-
   maplist(map_swc(true2,Prior), [A1,Pi]), % establish same shape as prior
   map_swc(mul_add(ITemp,1.0-ITemp), Prior, EffPrior),
   map_sum_sw(log_partition_dirichlet, Prior, LogZPrior),
   call(vb_helper(ITemp, LogZPrior, EffPrior), A1, Pi - Div),
   once(graph_counts(Stats, log, Graph, Pi, Eta, LL)),
   map_swc(mul_add(ITemp), EffPrior, Eta, A2),
   esc(sub,[LL,Div],[Obj]),
   make_lambda(Obj, A1, A2, Lambda).

vb_helper(ITemp, LogZPrior, EffPrior, A, Pi - Div) :-
   map_sw(mean_log_dirichlet, A, PsiA),
   map_swc(math:sub, EffPrior, A, Delta),
   map_swc(math:mul(ITemp), PsiA, Pi),
   map_sum_sw(log_part_dir, A, LogZA),
   map_sum_sw(map_sum(math:mul), PsiA, Delta, Diff),
   Div is Diff - LogZA + ITemp*LogZPrior.

mul_add(1.0,X,Y,Z) :- !, add(X,Y,Z).
mul_add(K,X,Y,Z) :- mul(K,Y,KY), add(X,KY,Z).

%! converge(+C:convergence, +L:pred(-learner), -LL:list(float), +P1:sw_params, -P2:sw_params) is det.
%  Use L to create a predicate to do one step of learning, and then iterate
%  this until convergence, starting from P1 and ending with P2. History of
%  objective function values is returned in LL. Convergence C is of type:
%  ==
%  convergence ---> abs(float); rel(float).
%  ==
:- meta_predicate converge(+,1,-,+,-).
converge(Test, Setup, [X0|History], S0, SFinal) :-
   debug(learn(setup), 'converge: Setting up...',[]),
   run_lambda_compiler((
      time(call(Setup, Step)),
      call(Step, X0, S0, S1),
      time(converge_x(Test, Step, X0, History, S1, SFinal)))).

converge_x(Test, Step, X0, [X1|History], S1, SFinal) :-
   debug(learn(iters), 'converge: Cost = ~p.',[X0]),
   call(Step, X1, S1, S2),
   (  converged(Test, X0, X1) -> History=[], SFinal=S2
   ;  converge_x(Test, Step, X1, History, S2, SFinal)
   ).

converged(abs(Eps), X1, X2) :- abs(X1-X2) =< Eps.
converged(rel(Del), X1, X2) :- abs((X1-X2)/(X1+X2)) =< Del.
