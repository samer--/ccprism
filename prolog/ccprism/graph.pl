:- module(ccp_graph, [ graph_switches/2, prune_graph/4, top_value/2
                     , semiring_graph_fold/4, graph_viterbi/4, graph_nviterbi/4, graph_inside/3
                     , tree_stats//1, tree_stats/2, accum_stats/3, graph_counts/5
                     , igraph_sample_tree/4, igraph_sample_tree/3
                     ]).

/** <module> Inference and statistics on explanation hypergraphs
   @tbd
   - CLP(Q/R) for semiring ops
   - Automatic differentiation for outside computation
*/

:- use_module(library(apply_macros)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lambda2)).
:- use_module(library(lazy),        [lazy_maplist/3, lazy_unfold_finite/4]).
:- use_module(library(math),        [stoch/3]).
:- use_module(library(listutils),   [cons//1, foldr/4, zip/3]).
:- use_module(library(callutils),   [mr/5, (*)/4, const/3, true1/1]).
:- use_module(library(data/pair),   [fst/2, snd/2]).
:- use_module(library(rbutils),     [rb_gen/3, rb_add//2, rb_app//2, rb_get//2]).
:- use_module(effects,   [dist/3]).
:- use_module(switches,  [map_swc/4]).
:- use_module(lazymath,  [ max/3, add/3, mul/3, exp/2, log_e/2, surp/2
                         , lse/2, stoch/2, log_stoch/2, map_sum/4, patient/4, lazy/4]).

:- set_prolog_flag(back_quotes, symbol_char).

top_value(Pairs, Top) :- memberchk((top:'$top$')-Top, Pairs).

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
pmap_sws(Map,SWs) :- setof(SW, V^X^rb_gen(SW:=V,X,Map), SWs) -> true; SWs=[].

:- meta_predicate pmap_collate(3,1,+,+,?).
pmap_collate(Conv,Def,Map,SW,SW-XX) :- 
   call(SW,_,Vals,[]), maplist(pmap_get(Conv,Def,Map,SW),Vals,XX).

pmap_get(Conv,Def,Map,SW,Val,X) :- 
   rb_lookup(SW:=Val, P, Map) -> call(Conv,SW:=Val,P,X); call(Def,X).

%% semiring_graph_fold(+SR:sr(A,B,C,T), +G:graph, ?P:params(T), -R:list(pair(goal,C))) is det.
%
%  Folds the semiring SR over the explanation graph G, resulting in R, a list of pairs
%  of goals in the original graph with the result of the fold for that goal. Different
%  semirings can produce many kinds of parsing analysis.
semiring_graph_fold(SR, Graph, Params, GoalSums) :- 
   rb_empty(E), 
   foldl(sr_sum(SR), Graph, GoalSums, E, Map), pmap_sws(Map, SWs),
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
sr_inj(kbest,     F, P, [Q-F]) :- surp(P,Q).
sr_inj(ann(SR),   F, P, Q-F)   :- sr_inj(SR,F,P,Q).
sr_inj(R1-R2,     F, P, Q1-Q2) :- sr_inj(R1,F,P,Q1), sr_inj(R2,F,P,Q2).

sr_proj(r(_,P,_,_), _, X, Y, Y) :- call(P,X,Y).
sr_proj(best(_),  G, X-E, X-E, X-(G-E)).
sr_proj(kbest,    G, X, X, Y)         :- freeze(Y,lazy_maplist(k_tag(G),X,Y)).
sr_proj(ann(SR),  G, X-Z, W-Z, Y-G)     :- sr_proj(SR,G,X,W,Y).
sr_proj(R1-R2,    G, X1-X2, Z1-Z2, Y1-Y2) :- sr_proj(R1,G,X1,Z1,Y1), sr_proj(R2,G,X2,Z2,Y2).

sr_plus(r(_,_,_,O), X) --> call(O,X).
sr_plus(best(_),  X) --> v_max(X).
sr_plus(kbest,    X) --> lazy(k_min,X).
sr_plus(ann(SR),  X-Expl) --> sr_plus(SR,X) <\> cons(X-Expl).
sr_plus(R1-R2,    X1-X2) --> sr_plus(R1,X1) <\> sr_plus(R2,X2).

sr_times(r(_,_,O,_), X) --> call(O,X).
sr_times(best(_),  X-F) --> add(X) <\> cons(F).
sr_times(kbest,    X) --> lazy(k_mul,X).
sr_times(ann(SR),  X-F) --> sr_times(SR,X) <\> cons(X-F).
sr_times(R1-R2,    X1-X2) --> sr_times(R1,X1) <\> sr_times(R2,X2).

sr_zero(r(_,_,_,O), I) :- m_zero(O,I).
sr_zero(best(_),  Z-_)   :- m_zero(max,Z).
sr_zero(kbest,    []).
sr_zero(ann(SR),  Z-[])  :- sr_zero(SR,Z).
sr_zero(R1-R2,    Z1-Z2) :- sr_zero(R1,Z1), sr_zero(R2,Z2).

sr_unit(r(_,_,O,_), I) :- m_zero(O,I).
sr_unit(best(_),  0-[]).
sr_unit(kbest,    [0-[]]).
sr_unit(ann(SR),  U-[])  :- sr_unit(SR,U).
sr_unit(R1-R2,    U1-U2) :- sr_unit(R1,U1), sr_unit(R2,U2).

m_zero(add,0).
m_zero(mul,1).
m_zero(max,-inf).
m_zero(cons,[]).

v_max(LX-X,LY-Y,Z) :- when(ground(LX-LY),(LX>=LY -> Z=LX-X; Z=LY-Y)).

% ---- lazy k-best algebra ----
k_tag(G,L-X,L-(G-X)). % tag explanaiton with head goal
k_min([],Y,Y) :- !.
k_min(X,[],X) :- !.
k_min([X|Xs],[Y|Ys],[Z|Zs]) :-
   (  LX-_=X, LY-_=Y, LX =< LY
   -> Z=X, freeze(Zs, k_min(Xs,[Y|Ys],Zs))
   ;  Z=Y, freeze(Zs, k_min([X|Xs],Ys,Zs))
   ).

k_mul(X,Y,Z) :-
   empty_set(EmptyS), empty_heap(EmptyQ),
   k_queue(0^X-0^Y, EmptyS-EmptyQ, TQ1),
   lazy_unfold_finite(k_next, Z, TQ1, _).

k_next(L-[XF|YFs]) -->
   \> pq_get(L,P),
   {P=I^[X0|X]-J^[Y0|Y], _-XF=X0, _-YFs=Y0}, 
   {succ(J,J1)}, k_queue(I^X-J1^[Y0|Y]),
   {succ(I,I1)}, k_queue(I1^[X0|X]-J^Y).

k_queue(P) --> {P=I^X-J^Y}, \< add_to_set(I-J), {k_cost(X,Y,L)} -> \> pq_add(L,P); [].
k_cost([X0-_|_],[Y0-_|_], L) :- L is X0+Y0.

pq_add(L,P,H1,H2) :- add_to_heap(H1,L,P,H2).
pq_get(L,P,H1,H2) :- get_from_heap(H1,L,P,H2).
add_to_set(X,S1,[X|S1]) :- \+memberchk(X,S1).
empty_set([]).

% ---------- inside and viterbi probs, explanation trees -----------
graph_inside(Graph, Params, IGraph)  :- 
   semiring_graph_fold(ann(r(=,=,mul,add)), Graph, Params, IGraph).
graph_viterbi(Graph, Params, Tree, LP) :- 
   semiring_graph_fold(best(lin), Graph, Params, VGraph), top_value(VGraph, LP-Tree).
graph_nviterbi(Graph, Params, Tree, LP) :-
   semiring_graph_fold(kbest, Graph, Params, VGraph), top_value(VGraph, Expls),
   member(LP-Tree,Expls).

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
graph_counts(vit, PScaling, Graph, P1, LP-Eta) :-
   call(top_value * semiring_graph_fold(best(PScaling),Graph), P1, LP-Tree),
   when(ground(LP), tree_stats(_-Tree, Eta)).

graph_counts(io(IScaling), PScaling, Graph, P1, LP-Eta) :-
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
