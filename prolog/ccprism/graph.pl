:- module(ccp_graph, [ graph_switches/2, prune_graph/4, top_value/2, top_goal/1
                     , graph_fold/4, graph_viterbi/4, graph_inside/3
                     , tree_stats/2, sw_trees_stats/3, accum_stats/3, graph_counts/6
                     , igraph_sample_tree/3, igraph_entropy/3
                     ]).

/** <module> Inference and statistics on explanation hypergraphs

   This module provides algorithms on explanation hypergraphs, based on the ideas
   of Sato (in PRISM), Klein and Manning [1] and Goodman [2]. Also provided
   are methods for sampling explanations from their posterior distribution [2] and
   computing the entropy of the posterior distribution using a method which,
   to my knowledge, has not been published before.

   [1] D. Klein and C. D. Manning. Parsing and hypergraphs.
   In New developments in parsing technology, pages 351–372. Springer, 2004.

   [2] J. Goodman. Parsing inside-out. PhD thesis,
       Division of Engineering and Applied Sciences, Harvard University, 1998.
   ==
   tree ---> goal - list(tree).
   igraph == f_graph(pair,float,float,float)
          == list(pair(goal,weighted(list(weighted(list(weighted(factor)))))))
   weighted(X) == pair(float,X).
   ==
*/

:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(lambdaki)).
:- use_module(library(typedef)).
:- use_module(library(math),        [stoch/3]).
:- use_module(library(listutils),   [cons//1, foldr/4, zip/3]).
:- use_module(library(callutils),   [mr/5, (*)/4, const/3, true1/1]).
:- use_module(library(data/pair),   [fst/2, snd/2]).
:- use_module(library(rbutils),     [rb_in/3, rb_add//2, rb_app//2, rb_get//2]).
:- use_module(library(autodiff2),   [back/1, deriv/3, compile/1]).
:- use_module(library(lazymath),    [max/3, add/3, mul/3, exp/2, log/2, lse/2, stoch/2, log_stoch/2]).
:- use_module(effects,   [dist/3]).
:- use_module(switches,  [map_swc/3, map_swc/4]).

:- multifile sr_inj/4, sr_proj/5, sr_times/4, sr_plus/4, sr_unit/2, sr_zero/2, m_zero/2.

:- type graph == list(pair(goal, list(list(factor)))).
:- type counts_method ---> vit; io(scaling).
:- type scaling ---> lin; log.

%! top_value(+Pairs:list(pair(goal,A)), -X:A) is semidet.
%  Extract the value associated with the goal =|'^top':top|= from a list
%  of goal-value pairs. This can be applied to explanation graphs or
%  the results of graph_fold/4.
top_value(Pairs, Top) :- memberchk(('^top':top)-Top, Pairs).
top_goal('^top':top).


%! prune_graph(+P:pred(+tcall(F,_,D),-D), +Top:goal, +G1:f_graph(F,A,B,C), -G2:f_graph(F,A,B,C)) is det.
%  ==
%  f_graph(F,A,B,C) == list(pair(goal,tcall(F,A,list(tcall(F,B,list(tcall(F,C,factor)))))))
%  ==
%  Prune a graph or annotated graph to keep only goals reachable from a given top goal.
%  With apologies, the type is quite complicated. The input and output graphs are lists of goals paired
%  with _annotated_ explanations. The type of an annotation is described by the type constructor
%  F: =|F(E,D)|= is the type of a =|D|= annotated with an =|E|=. The first argument P knows how to strip
%  off any type of annotation and return the =|D|=. This is how we dig down into the annotated explanations
%  to find out which subgoals are referenced. For example, if =|F = pair|=, then P should be =|snd|=.
%  If =|F(E,D) = D|= (ie no annotation), then P should be (=). Since PlDoc won't accept high-order
%  type terms, we write =|F(E,D)|= as =|tcall(F,E,D)|=, where =|tcall|= is like =|call|= for types.
prune_graph(Mapper, Top, GL1, GL2) :-
   list_to_rbtree(GL1,G1),
   rb_empty(E), children(Top,Mapper,G1,E,G2),
   rb_visit(G2,GL2).

% SA 2017/10 - Temporarily weaken pattern matching to handle arbitrary factors
% children(_:=_, _, _) --> !.
% children(\_,   _, _) --> !.
children(Mod:Goal,  M, G) --> !,
   {rb_lookup(Mod:Goal,Entry,G)}, rb_add(Mod:Goal,Entry),
   {call(M, Entry, Expls)},
   foldl(mr(M,foldl(mr(M,new_children(M,G)))),Expls).
children(_,  _, _) --> !.

new_children(M, G, F) -->
   rb_get(F,_) -> []; children(F,M,G).

%! graph_switches(+G:graph, -SWs:list(switch(_))) is det.
%  Extract list of switches referenced in an explanation graph.
graph_switches(G,SWs) :- (setof(SW, graph_sw(G,SW), SWs) -> true; SWs=[]).
graph_sw(G,SW)        :- member(_-Es,G), member(E,Es), member(SW:=_,E).


%! graph_fold(+SR:sr(A,B,C,T), ?P:params(T), +G:graph, -R:list(pair(goal,W))) is det.
%
%  Folds the semiring SR over the explanation graph G. Produces R, a list of pairs
%  of goals in the original graph with the result of the fold for that goal. Different
%  semirings can produce many kinds of parsing analysis. The algebra is not strictly a
%  semiring, as the times and plus operators have types =|A, B -> B|= and =|B, C -> C|=
%  respectively as this makes it easier to avoid unnecessary operations like list appending.
%
%  An algebra of type =|sr(A,B,C,T)|= must provide 4 operators and 2 values:
%  ==
%  inject  : T, factor -> A
%  times   : A, B -> B
%  plus    : B, C -> C
%  project : goal, C -> A, W
%  unit    : B
%  zero    : C
%  ==
%  Semirings are extensible using multifile predicates sr_inj/4, sr_proj/5, sr_times/4,
%  sr_plus/4, sr_unit/2 and sr_zero/2.
%
%  Available semirings in this module:
%     * r(pred(+T,-A), pred(+C,-C), pred(+A,+B,-B), pred(+B,+C,-C))
%     A term containing the operators in restricted forms as callable terms.
%     The unit and zero for the times and plus operators respectively are looked up in m_zero/2.
%     * best
%     Finds the best single explanation for each goal. Parameters are assumed to be probabilities.
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

graph_fold(SR, Params, Graph, GoalSums) :-
   rb_empty(E),
   foldl(sr_sum(SR), Graph, GoalSums, E, Map),
   fmap_sws(Map, SWs),
   maplist(fmap_collate_sw(sr_param(SR),true1,Map),SWs,Params).

sr_sum(SR, Goal-Expls, Goal-Result) -->
   fmap(Goal,Proj), {sr_zero(SR,Zero)},
   run_right(foldr(sr_add_prod(SR),Expls), Zero, Sum),
   {sr_proj(SR,Goal,Sum,Proj,Result)}.

sr_add_prod(SR, Expl) -->
   {sr_unit(SR,Unit)},
   run_right(foldr(sr_factor(SR), Expl), Unit, Prod) <\> sr_plus(SR,Prod).

sr_factor(SR, M:Head)  --> !, fmap(M:Head,X) <\> sr_times(SR,X).
sr_factor(SR, SW:=Val) --> !, fmap(SW:=Val,X) <\> sr_times(SR,X).
sr_factor(SR, \P)      --> {sr_inj(SR,P,\P,X)}, \> sr_times(SR,X).

sr_param(SR,F,X,P) :- sr_inj(SR,P,F,X), !.

% --------- semirings ---------
sr_inj(id,        _, F, F).
sr_inj(r(I,_,_,_),  P, _, X)   :- call(I,P,X).
sr_inj(best,      P, F, Q-F)   :- log(P,Q).
sr_inj(ann(SR),   P, F, Q-F)   :- sr_inj(SR,P,F,Q).
sr_inj(R1-R2,     P, F, Q1-Q2) :- sr_inj(R1,P,F,Q1), sr_inj(R2,P,F,Q2).

sr_proj(id,       G, Z,   G, Z).
sr_proj(r(_,P,_,_), _, X, Y, Y) :- call(P,X,Y).
sr_proj(best,     G, X-E, X-(G-E), X-E).
sr_proj(ann(SR),  G, X-Z, Y-G, W-Z)       :- sr_proj(SR,G,X,Y,W).
sr_proj(R1-R2,    G, X1-X2, Y1-Y2, Z1-Z2) :- sr_proj(R1,G,X1,Y1,Z1), sr_proj(R2,G,X2,Y2,Z2).

sr_plus(id,       Expl) --> cons(Expl).
sr_plus(r(_,_,_,O), X) --> call(O,X).
sr_plus(best,     X) --> v_max(X).
sr_plus(ann(SR),  X-Expl) --> sr_plus(SR,X) <\> cons(X-Expl).
sr_plus(R1-R2,    X1-X2) --> sr_plus(R1,X1) <\> sr_plus(R2,X2).

sr_times(id,       F)   --> cons(F).
sr_times(r(_,_,O,_), X) --> call(O,X).
sr_times(best,     X-F) --> add(X) <\> cons(F).
sr_times(ann(SR),  X-F) --> sr_times(SR,X) <\> cons(X-F).
sr_times(R1-R2,    X1-X2) --> sr_times(R1,X1) <\> sr_times(R2,X2).

sr_zero(id,       []).
sr_zero(r(_,_,_,O), I) :- m_zero(O,I).
sr_zero(best,     Z-_)   :- m_zero(max,Z).
sr_zero(ann(SR),  Z-[])  :- sr_zero(SR,Z).
sr_zero(R1-R2,    Z1-Z2) :- sr_zero(R1,Z1), sr_zero(R2,Z2).

sr_unit(id,       []).
sr_unit(r(_,_,O,_), I) :- m_zero(O,I).
sr_unit(best,     0.0-[]).
sr_unit(ann(SR),  U-[])  :- sr_unit(SR,U).
sr_unit(R1-R2,    U1-U2) :- sr_unit(R1,U1), sr_unit(R2,U2).

m_zero(add,0.0).
m_zero(mul,1.0).
m_zero(max,-inf).
m_zero(cons,[]).
m_zero(autodiff2:mul,1.0).
m_zero(autodiff2:add,0.0).
m_zero(autodiff2:add_to_wsum,0.0).
m_zero(autodiff2:max,-inf).

v_max(LX-X,LY-Y,Z) :- when(ground(LX-LY),(LX>=LY -> Z=LX-X; Z=LY-Y)).

%! graph_inside(+G:graph, ?P:sw_params, -IG:igraph) is det.
graph_inside(Graph, Params, IGraph)  :-
   graph_fold(ann(r(=,=,mul,add)), Params, Graph, IGraph).

%! graph_viterbi(+G:graph, ?P:sw_params, -T:list(tree), -LP:float) is det.
%  Compute Viterbi (most likely) explanation, returning the list of children
%  of the top node, since the top goal itself is fixed.
graph_viterbi(Graph, Params, Tree, LP) :-
   graph_fold(best, Params, Graph, VGraph), top_value(VGraph, LP-Tree).

%! igraph_sample_tree(+IG:igraph, +H:goal, -Ts:list(tree)) is det.
%
%  Uses prob effect to sample a tree from a graph annotated with inside
%  probabilities, as produced by graph_inside/3/
igraph_sample_tree(Graph, Head, Subtrees) :-
   memberchk(Head-(_-Expls), Graph), % Head should be unique in graph
   zip(Ps,Es,Expls), stoch(Ps,Ps1,_), dist(Ps1,Es,Expl),
   maplist(sample_subexpl_tree(Graph), Expl, Subtrees).

sample_subexpl_tree(G, _-(M:Goal), (M:Goal)-Tree) :- !, igraph_sample_tree(G, M:Goal, Tree).
sample_subexpl_tree(_, _-Factor,   Factor).

%! igraph_entropy(+S:scaling, +IG:igraph, -Es:list(pair(goal,float))) is det.
%  Explanation entropies from annotated explanation graph.
igraph_entropy(Scaling, IGraph, GoalEntropies) :-
   rb_empty(E),
   foldl(goal_entropy(Scaling), IGraph, GoalEntropies, E, Map),
   rb_visit(Map, GoalEntropies).

goal_entropy(Scaling, Goal-(_ - WeightedExpls), Goal-Entropy) -->
   fmap(Goal,Entropy),
   {zip(Ws, Es, WeightedExpls), scaling_stoch(Scaling, Ws, Ps)},
   run_right(foldl(expl_entropy(Scaling),Ps,Es), 0.0, Entropy).

scaling_stoch(lin,X,Y) :- stoch(X,Y).
scaling_stoch(log,X,Y) :- log_stoch(X,Y).

expl_entropy(Scaling, Pe, Expl) -->
   {when(ground(FactorsEntropy-Pe), expl_entropy(Scaling, Pe, FactorsEntropy, ExplEntropy))},
   run_right(foldl(mr(snd,factor_entropy),Expl), 0.0, FactorsEntropy) <\> add(ExplEntropy).

expl_entropy(lin, Pe, HFactors, HE) :- HE is Pe*(HFactors - log(Pe)).
expl_entropy(log, Pe, HFactors, HE) :- HE is exp(Pe)*(HFactors - Pe).

factor_entropy(M:Head) --> !, fmap(M:Head,H) <\> add(H).
factor_entropy(_) --> [].


%! graph_counts(+Meth:counts_method, +PSc:scaling, +G:graph, P:sw_params, C:sw_params, LP:float) is det.
%
%  Compute expected switch counts C from explanation graph G with switch parameters
%  P. Uses automatic differentiation of the expression for the log of the inside
%  probability LP of the graph. Params can be unbound - binding them later triggers
%  the computations required to yield numerical values in the result.
%  ==
%  counts_method ---> io(scaling); vit.
%  ==
graph_counts(Method, PSc, Graph, Params, Eta, LogProb) :-
   method_scaling_semiring(Method, ISc, SR, ToLogProb),
   graph_fold(SR, P0, Graph, IG), autodiff2:expand_wsums,
   call(ToLogProb*top_value, IG, LogProb),
   scaling_log_params(ISc, PSc, P0, Params0, LogP0),
   map_swc(deriv(LogProb), LogP0, Eta),
   back(LogProb), compile(_), Params=Params0.

method_scaling_semiring(vit,     log, r(=,=,autodiff2:add,autodiff2:max), =).
method_scaling_semiring(io(lin), lin, r(=,=,autodiff2:mul,autodiff2:add), autodiff2:log).
method_scaling_semiring(io(log), log, r(=,autodiff2:lse, autodiff2:add,cons), =).
method_scaling_semiring(io(log_wsum), log, r(=,autodiff2:lse, autodiff2:add_to_wsum,cons), =).

scaling_log_params(lin, lin, P0,    P0,    LogP0) :- map_swc(autodiff2:llog, P0, LogP0).
scaling_log_params(lin, log, P0,    LogP0, LogP0) :- map_swc(autodiff2:exp, LogP0, P0).
scaling_log_params(log, lin, LogP0, P0,    LogP0) :- map_swc(autodiff2:log, P0, LogP0).
scaling_log_params(log, log, LogP0, LogP0, LogP0).

%! accum_stats(+Acc:pred(fmap(int), fmap(int)), +GSWs:pred(fmap(int), list(switch(_))), -Stats:sw_params) is det.
:- meta_predicate accum_stats(//,2,-).
accum_stats(Acc, GetSWs, Stats) :-
   rb_empty(C0),
   call_dcg(Acc,C0,C1), call(GetSWs,C1,SWs),
   maplist(fmap_collate_sw(right,=(0),C1),SWs,Stats).

%! tree_stats(+T:tree, -C:sw_params) is det.
tree_stats(Tree,Counts) :- accum_stats(tree_stats(Tree), fmap_sws, Counts).

sw_trees_stats(SWs,Trees,Stats) :- accum_stats(tree_stats(_-Trees),const(SWs),Stats).

tree_stats(_-Subtrees) --> foldl(subtree_stats,Subtrees).
subtree_stats(_-Trees) --> foldl(subtree_stats,Trees).
subtree_stats(SW:=Val) --> rb_app(SW:=Val,succ) -> []; rb_add(SW:=Val,1).
subtree_stats(\_)      --> [].
right(_,X,X).

% --- Factor-value map, used internally --------------
% =| fmap(A) == rbtree(factor, A).

%! fmap(+F:factor, ?X:A, +M1:fmap(A), -M2:fmap(A)) is det.
%  Unify X with value under K in M1 if present, otherwise add it.
fmap(X,Y) --> rb_add(X,Y) -> []; rb_get(X,Y).

%! fmap_sws(+M:fmap(A), -SWs:list(switch(_))) is det.
%  Collect sorted list of switches from keys in M.
fmap_sws(Map,SWs) :- rb_fold(emit_if_sw,Map,SWs1,[]), sort(SWs1,SWs).
emit_if_sw(F-_) --> {F=(SW:=_)} -> [SW]; [].

%! fmap_collate_sw(+Conv:pred(factor,+A,-B), +Def:pred(-B), +M:fmap(A), +SW:switch(_), -SWX:pair(switch(_), list(B))) is det.
%  Collect parameter data for each value of a switch. Either the data is
%  extracted from the map and converted using Conv, or created using Def.
:- meta_predicate fmap_collate_sw(3,1,+,+,?).
fmap_collate_sw(Conv,Def,Map,SW,SW-XX) :-
   call(SW,_,Vals,[]), maplist(sw_val_or_default(Conv,Def,Map,SW),Vals,XX).

sw_val_or_default(Conv,Def,Map,SW,Val,X) :-
   rb_lookup(SW:=Val, P, Map) -> call(Conv,SW:=Val,P,X); call(Def,X).
