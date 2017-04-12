:- module(ccp_graph, [ graph_switches/2, prune_graph/4, top_value/2
                     , semiring_graph_fold/4, graph_viterbi/4, graph_inside/3
                     , tree_stats//1, tree_stats/2, accum_stats/3, graph_counts/6
                     , igraph_sample_tree/4, igraph_sample_tree/3
                     ]).

/** <module> Inference and statistics on explanation hypergraphs */

:- use_module(library(apply_macros)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lambda2)).
:- use_module(library(typedef)).
:- use_module(library(math),        [stoch/3]).
:- use_module(library(listutils),   [cons//1, foldr/4, zip/3]).
:- use_module(library(callutils),   [mr/5, (*)/4, const/3, true1/1]).
:- use_module(library(data/pair),   [fst/2, snd/2]).
:- use_module(library(rbutils),     [rb_gen/3, rb_add//2, rb_app//2, rb_get//2]).
:- use_module(effects,   [dist/3]).
:- use_module(switches,  [map_swc/4]).
:- use_module(lazymath,  [max/3, add/3, mul/3, exp/2, log_e/2, lse/2, stoch/2, log_stoch/2, map_sum/4, patient/4]).

:- set_prolog_flag(back_quotes, symbol_char).

:- multifile sr_inj/4, sr_proj/5, sr_times/4, sr_plus/4, sr_unit/2, sr_zero/2.

:- type graph == list(pair(goal, list(list(factor)))).
:- type counts_method ---> vit; io(scaling).
:- type scaling ---> lin; log.

%% top_value(+Pairs:list(pair(goal,A)), -X:A) is semidet.
%  Extract the value associated with the goal =|top:'$top$'|= from a list
%  of goal-value pairs. This can be applied to explanation graphs or
%  the results of semiring_graph_fold/4.
top_value(Pairs, Top) :- memberchk((top:'$top$')-Top, Pairs).

%% prune_graph(+P:pred(+A,-list(_)), +Top:goal, G1:list(pair(goal),A), G2:list(pair(goal),A)) is det.
%  Prune a graph or annotated graph to keep only goals reachable from a given top goal.
%  With apologies, the first argument is so badly typed, I cannot really explain what it does...
%  @tbd Fix this so it can be explained.
prune_graph(Mapper, Top, GL1, GL2) :-
   list_to_rbtree(GL1,G1), 
   rb_empty(E), children(Top,Mapper,G1,E,G2),
   rb_visit(G2,GL2).

children(_:=_, _, _) --> !.
children(@_,   _, _) --> !.
children(Top,  M, G) --> 
   {rb_lookup(Top,Entry,G)}, rb_add(Top,Entry),
   {call(M, Entry, Expls)},
   foldl(mr(M,foldl(mr(M,new_children(M,G)))),Expls).
new_children(M, G, F) -->
   rb_get(F,_) -> []; children(F,M,G).

%% graph_switches(+G:graph, -SWs:list(switch(_))) is det.
%  Extract list of switches referenced in an explanation graph.
graph_switches(G,SWs) :- (setof(SW, graph_sw(G,SW), SWs) -> true; SWs=[]). 
graph_sw(G,SW)        :- member(_-Es,G), member(E,Es), member(SW:=_,E).

% --------- switch-value map -----------
pmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).
% pmap_sws(Map,SWs) :- setof(SW, Map^V^X^rb_gen(SW:=V,X,Map), SWs) -> true; SWs=[].
pmap_sws(Map,SWs) :- rb_fold(pmap_entry_sw,Map,SWs1,[]), sort(SWs1,SWs).
pmap_entry_sw(F-_) --> {F=(SW:=_)} -> [SW]; [].

:- meta_predicate pmap_collate(3,1,+,+,?).
pmap_collate(Conv,Def,Map,SW,SW-XX) :- 
   call(SW,_,Vals,[]), maplist(pmap_get(Conv,Def,Map,SW),Vals,XX).

pmap_get(Conv,Def,Map,SW,Val,X) :- 
   rb_lookup(SW:=Val, P, Map) -> call(Conv,SW:=Val,P,X); call(Def,X).


%% semiring_graph_fold(+SR:sr(A,B,C,T), +G:graph, ?P:params(T), -R:list(pair(goal,C))) is det.
%
%  Folds the semiring SR over the explanation graph G. Produces R, a list of pairs
%  of goals in the original graph with the result of the fold for that goal. Different
%  semirings can produce many kinds of parsing analysis. The algebra is not strictly a 
%  semiring, as the times and plus operators have types =|A, B -> B|= and =|B, C -> C|=
%  respectively as this makes it easier to avoid unnecessary operations like list appending.
%
%  An algebra of type =|sr(A,B,C,T)|= must provide 4 operators and 2 values:
%  ==
%  inject  : factor, T -> A
%  times   : A, B -> B
%  plus    : B, C -> C
%  project : goal, C -> C, A
%  unit    : B
%  zero    : C
%  ==
%  Semirings are extensible using multifile predicates sr_inj/4, sr_proj/5, sr_times/4,
%  sr_plus/4, sr_unit/2 and sr_zero/2.
%
%  Available semirings in this module:
%     * r(pred(+T,-A), pred(+C,-C), pred(+A,+B,-B), pred(+B,+C,-C))
%     A term containing the 4 restricted operators as callable terms.
%     * best(scaling)
%     Finds the best single explanation for each goal. If scaling is 'lin', parameters 
%     are assumed to be probabilities; if it's 'log', they are assumed to be log probabilities.
%     * ann(sr(A,B,C,T))
%     Annotates the original hypergraph with the results of any semiring analysis.
%     *  sr(A1,B1,C1,T) - sr(A1,B1,C1,T)
%     For each goal, return a pair of results from any two semiring analyses.
%
%  Various standard analysis can be obtained by using the appropriate semiring:
%     * r(=,=,mul,add)
%     Inside algorithm from  linear probabilities.
%     * r(=,lse,add,cons)
%     Inside algorithm with log-scaling from log probabilities
%     * r(=,=,mul,max)
%     Viterbi probabilities.

semiring_graph_fold(SR, Graph, Params, GoalSums) :- 
   rb_empty(E), 
   foldl(sr_sum(SR), Graph, GoalSums, E, Map), 
   pmap_sws(Map, SWs),
   maplist(pmap_collate(sr_param(SR),true1,Map),SWs,Params).

sr_sum(SR, Goal-Expls, Goal-Sum1) -->
   pmap(Goal,Proj),
   {sr_zero(SR,Zero), sr_proj(SR,Goal,Sum,Sum1,Proj)}, !, 
   run_right(foldr(sr_add_prod(SR),Expls), Zero, Sum).

sr_add_prod(SR, Expl) --> 
   {sr_unit(SR,Unit)}, !, 
   run_right(foldr(sr_factor(SR), Expl), Unit, Prod) <\> sr_plus(SR,Prod), !. 

sr_factor(SR, M:Head)  --> pmap(M:Head,X) <\> sr_times(SR,X), !. 
sr_factor(SR, SW:=Val) --> pmap(SW:=Val,X) <\> sr_times(SR,X), !. 
sr_factor(SR, @P)      --> {sr_inj(SR,const,P,X)}, \> sr_times(SR,X), !.

sr_param(SR,F,X,P) :- sr_inj(SR,F,P,X).

% --------- semirings ---------
sr_inj(r(I,_,_,_),  _, P, X)     :- call(I,P,X).
sr_inj(best(log), F, P, P-F).
sr_inj(best(lin), F, P, Q-F)   :- log_e(P,Q).
sr_inj(ann(SR),   F, P, Q-F)   :- sr_inj(SR,F,P,Q).
sr_inj(R1-R2,     F, P, Q1-Q2) :- sr_inj(R1,F,P,Q1), sr_inj(R2,F,P,Q2).

sr_proj(r(_,P,_,_), _, X, Y, Y) :- call(P,X,Y).
sr_proj(best(_),  G, X-E, X-E, X-(G-E)).
sr_proj(ann(SR),  G, X-Z, W-Z, Y-G)     :- sr_proj(SR,G,X,W,Y).
sr_proj(R1-R2,    G, X1-X2, Z1-Z2, Y1-Y2) :- sr_proj(R1,G,X1,Z1,Y1), sr_proj(R2,G,X2,Z2,Y2).

sr_plus(r(_,_,_,O), X) --> call(O,X).
sr_plus(best(_),  X) --> v_max(X).
sr_plus(ann(SR),  X-Expl) --> sr_plus(SR,X) <\> cons(X-Expl).
sr_plus(R1-R2,    X1-X2) --> sr_plus(R1,X1) <\> sr_plus(R2,X2).

sr_times(r(_,_,O,_), X) --> call(O,X).
sr_times(best(_),  X-F) --> add(X) <\> cons(F).
sr_times(ann(SR),  X-F) --> sr_times(SR,X) <\> cons(X-F).
sr_times(R1-R2,    X1-X2) --> sr_times(R1,X1) <\> sr_times(R2,X2).

sr_zero(r(_,_,_,O), I) :- m_zero(O,I).
sr_zero(best(_),  Z-_)   :- m_zero(max,Z).
sr_zero(ann(SR),  Z-[])  :- sr_zero(SR,Z).
sr_zero(R1-R2,    Z1-Z2) :- sr_zero(R1,Z1), sr_zero(R2,Z2).

sr_unit(r(_,_,O,_), I) :- m_zero(O,I).
sr_unit(best(_),  0-[]).
sr_unit(ann(SR),  U-[])  :- sr_unit(SR,U).
sr_unit(R1-R2,    U1-U2) :- sr_unit(R1,U1), sr_unit(R2,U2).

m_zero(add,0).
m_zero(mul,1).
m_zero(max,-inf).
m_zero(cons,[]).

v_max(LX-X,LY-Y,Z) :- when(ground(LX-LY),(LX>=LY -> Z=LX-X; Z=LY-Y)).

% ---------- inside and viterbi probs, explanation trees -----------
graph_inside(Graph, Params, IGraph)  :- 
   semiring_graph_fold(ann(r(=,=,mul,add)), Graph, Params, IGraph).
graph_viterbi(Graph, Params, Tree, LP) :- 
   semiring_graph_fold(best(lin), Graph, Params, VGraph), top_value(VGraph, LP-Tree).

igraph_sample_tree(Graph, Tree, LogProb) :-
   igraph_sample_tree(Graph, top:'$top$', Tree, LogProb).
igraph_sample_tree(Graph, Head, Head - Subtrees, LogProb) :-
   memberchk(Head-(_-Expls), Graph), % Head should be unique in graph
   zip(Ps,Es,Expls), stoch(Ps,Ps1,_), dist(Ps1,Es,Expl),
   map_sum(sample_subexpl_tree(Graph), Expl, Subtrees, LogProb). 

sample_subexpl_tree(G, _-(M:Goal),  Tree,    LP) :- !, igraph_sample_tree(G, M:Goal, Tree, LP).
sample_subexpl_tree(_, P-(SW:=Val), SW:=Val, LP) :- !, LP is log(P).
sample_subexpl_tree(_, P-const,     const,   LP) :- LP is log(P).

% ---- explanation entropy ----
inside_graph_entropy(Scaling, IGraph, GoalEntropies) :- 
   rb_empty(E), 
   foldl(goal_entropy(Scaling), IGraph, GoalEntropies, E, Map), 
   rb_visit(Map, GoalEntropies).

goal_entropy(Scaling, Goal-(_ - WeightedExpls), Goal-Entropy) -->
   pmap(Goal,Entropy),
   {zip(Ws, Es, WeightedExpls), scaling_stoch(Scaling, Ws, Ps)},
   run_right(foldl(expl_entropy(Scaling),Ps,Es), 0, Entropy).

scaling_stoch(lin,X,Y) :- stoch(X,Y).
scaling_stoch(log,X,Y) :- log_stoch(X,Y).

expl_entropy(Scaling, Pe, Expl) --> 
   {when(ground(FactorsEntropy-Pe), expl_entropy(Scaling, Pe, FactorsEntropy, ExplEntropy))},
   run_right(foldl(mr(snd,factor_entropy),Expl), 0, FactorsEntropy) <\> add(ExplEntropy). 

expl_entropy(lin, Pe, HFactors, HE) :- HE is Pe*(HFactors - log(Pe)).
expl_entropy(log, Pe, HFactors, HE) :- HE is exp(Pe)*(HFactors - Pe).

factor_entropy(M:Head) --> !, pmap(M:Head,H) <\> add(H).
factor_entropy(_) --> []. 

% --------- outside probabilities, ESS ----------------

%% graph_counts(+M:counts_method, +Sc:scaling, +G:graph, P:sw_params, C:sw_params, LP:number) is det.
%  Compute expected switch counts C from graph G with switch parameters P. Also
%  returns log probability of the graph in LP. Can do either inside-outside inference or
%  Viterbi inference, depending on M.
graph_counts(vit, PScaling, Graph, P1, Eta, LP) :-
   call(top_value * semiring_graph_fold(best(PScaling),Graph), P1, LP-Tree),
   when(ground(LP), tree_stats(_-Tree, Eta)).

graph_counts(io(IScaling), PScaling, Graph, P1, Eta, LP) :-
   i_scaling_info(IScaling, Min, TopBeta, TopAlpha, LP),
   scaling_info(IScaling/PScaling, SR, MakeCounts),
   semiring_graph_fold(ann(SR), Graph, P1, InsideG),
   top_value(InsideG, TopBeta-_), 
   foldl(soln_edges, InsideG, QCs, []), 
   call(group_pairs_by_key * keysort, QCs, InvGraph),
   rb_empty(Empty), pmap(top:'$top$',TopAlpha, Empty, Map1),
   foldl(q_alpha(IScaling), InvGraph, Map1, Map2),
   maplist(pmap_collate(right,=(Min),Map2)*fst, P1, Grad),
   map_swc(patient(MakeCounts), P1, Grad, Eta).
right(_,X,X).

i_scaling_info(lin, 0,    Pin, 1/Pin, LP) :- log_e(Pin,LP).
i_scaling_info(log, -inf, LP, -LP, LP).

scaling_info(lin/lin, r(=,=,mul,add),        math:mul).
scaling_info(lin/log, r(exp,=,mul,add),      \\X`Y`Z`(Z is exp(X)*Y)).
scaling_info(log/lin, r(log_e,lse,add,cons), \\X`Y`Z`(Z is X*exp(Y))).
scaling_info(log/log, r(=,lse,add,cons),     \\X`Y`Z`(Z is exp(X+Y))).

opt(Opts, Opt) :- option(Opt, Opts, _).
soln_edges(P-(_-Expls)) --> foldl(expl_edges(P),Expls).
expl_edges(P,Pe-Expl)       --> foldl(factor_edge(Pe,P),Expl).
factor_edge(Pe,P,BetaQ-Q)   --> [Q-qc(BetaQ,Pe,P)].

q_alpha(lin,Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha, QCs), 0, AlphaQ).
q_alpha(log,Q-QCs) --> pmap(Q, AlphaQ), run_right(foldl(qc_alpha_log, QCs), [], Alphas), 
                       {lse(Alphas,AlphaQ)}.

qc_alpha(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> add(AlphaQC),
   { when(ground(BetaQ), ( BetaQ =:= 0 -> AlphaQC=0
                         ; mul(AlphaP,Pe/BetaQ,AlphaQC))) }.

qc_alpha_log(qc(BetaQ,Pe,P)) --> 
   pmap(P, AlphaP) <\> cons(AlphaQC),
   { when(ground(BetaQ), ( BetaQ =:= -inf -> AlphaQC= -inf
                         ; add(AlphaP,Pe-BetaQ,AlphaQC))) }.

:- meta_predicate accum_stats(//,-), accum_stats(//,+,-).
accum_stats(Pred,Stats) :- 
   rb_empty(C0), 
   call_dcg(Pred,C0,C1), pmap_sws(C1, SWs),
   maplist(ccp_graph:pmap_collate(right,=(0),C1),SWs,Stats).
accum_stats(Pred,SWs,Stats) :- 
   rb_empty(C0), 
   call_dcg(Pred,C0,C1),
   maplist(ccp_graph:pmap_collate(right,=(0),C1),SWs,Stats).

tree_stats(Tree,Counts) :- accum_stats(tree_stats(Tree),Counts).

tree_stats(_-Subtrees) --> foldl(subtree_stats,Subtrees).
subtree_stats(_-Trees) --> foldl(subtree_stats,Trees).
subtree_stats(SW:=Val) --> rb_app(SW:=Val,succ) -> []; rb_add(SW:=Val,1).
subtree_stats(const) --> [].
