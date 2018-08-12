:- module(ccp_learn, [converge/5, learn/4, learn/5]).

/** <module> Expectation-maximisation, variational Bayes and deterministic annealing.
*/

:- use_module(library(math),       []).
:- use_module(library(callutils),  [(*)/4, true2/2]).
:- use_module(library(plrand),     [mean_log_dirichlet/2, log_partition_dirichlet/2]).
:- use_module(lazymath, [sub/3, max/3, add/3, mul/3, pow/3, stoch/2, map_sum/4, patient/3]).
:- use_module(graph,    [graph_counts/6]).
:- use_module(switches, [ map_sw/3, map_swc/3, map_swc/4, map_sum_sw/3, map_sum_sw/4
                        , sw_log_prob/3, sw_posteriors/3, sw_mode/2]).


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
learn(Method, StatsMethod, Graph, Step) :- learn(Method, StatsMethod, 1.0, Graph, Step).

learn(ml, Stats, ITemp, Graph, ccp_learn:unify3(t(P1,P2,LL))) :-
   once(graph_counts(Stats, lin, Graph, PP, Eta, LL)),
   map_swc(pow(ITemp), P1, PP),
   map_sw(stoch, Eta, P2).

learn(map(Prior), Stats, ITemp, Graph, ccp_learn:unify3(t(P1,P2,Obj))) :-
   once(graph_counts(Stats, lin, Graph, PP, Eta, LL)),
   patient(mul(ITemp)*sw_log_prob(Prior), P1, LP),
   sw_posteriors(Prior, Eta, Post), sw_mode(Post, P2),
   add(LL,LP,Obj), map_swc(pow(ITemp), P1, PP).

learn(vb(Prior), Stats, ITemp, Graph, ccp_learn:unify3(t(A1,A2,Obj))) :-
   maplist(map_swc(true2,Prior), [A1,Pi]), % establish same shape as prior
   map_swc(mul_add(ITemp,1.0-ITemp), Prior, EffPrior),
   map_sum_sw(log_partition_dirichlet, Prior, LogZPrior),
   patient(vb_helper(ITemp, LogZPrior, EffPrior), A1, Pi - Div),
   once(graph_counts(Stats, log, Graph, Pi, Eta, LL)),
   map_swc(mul_add(ITemp), EffPrior, Eta, A2),
   sub(Div,LL,Obj).

vb_helper(ITemp, LogZPrior, EffPrior, A, Pi - Div) :-
   map_sw(mean_log_dirichlet, A, PsiA),
   map_swc(math:sub, EffPrior, A, Delta),
   map_swc(mul(ITemp), PsiA, Pi),
   map_sum_sw(log_partition_dirichlet, A, LogZA),
   map_sum_sw(map_sum(math:mul), PsiA, Delta, Diff),
   Div is Diff - LogZA + ITemp*LogZPrior.

mul_add(1.0,X,Y,Z) :- !, when(ground(Y), Z is X+Y).
mul_add(K,X,Y,Z) :- when(ground(Y), Z is X+K*Y).
unify3(PStats,LP,P1,P2) :- copy_term(PStats, t(P1,P2,LP)).

%! converge(+C:convergence, +L:pred(-learner), -LL:list(float), +P1:sw_params, -P2:sw_params) is det.
%  Use L to create a predicate to do one step of learning, and then iterate
%  this until convergence, starting from P1 and ending with P2. History of
%  objective function values is returned in LL. Convergence C is of type:
%  ==
%  convergence ---> abs(float); rel(float).
%  ==
:- meta_predicate converge(+,1,-,+,-).
converge(Test, Setup, [X0|History], S0, SFinal) :-
   debug(learn, 'converge: Setting up...',[]),
   time(call(Setup, Step)), autodiff2:ops_count(N,N),
   debug(learn(setup), 'converge: Computation graph contains ~d ops.', [N]),
   call(Step, X0, S0, S1),
   time(converge_x(Test, Step, X0, History, S1, SFinal)).
converge_x(Test, Step, X0, [X1|History], S1, SFinal) :-
   debug(learn(iters), 'converge: Cost = ~p.',[X0]),
   call(Step, X1, S1, S2),
   (  converged(Test, X0, X1) -> History=[], SFinal=S2
   ;  converge_x(Test, Step, X1, History, S2, SFinal)
   ).

converged(abs(Eps), X1, X2) :- abs(X1-X2) =< Eps.
converged(rel(Del), X1, X2) :- abs((X1-X2)/(X1+X2)) =< Del.

