:- module(ccp_mcmc, [mc_evidence/4, mh_machine/4, gibbs_machine/5]).

/** <module> Gibbs and Metropolis-Hastings explanation samplers */

:- use_module(library(insist)).
:- use_module(library(callutils),   [(*)/4]).
:- use_module(library(listutils),   [enumerate/2]).
:- use_module(library(math),        [neg/2, add/3, sub/3, exp/2]).
:- use_module(library(data/pair),   [is_pair/1, pair/3, fst/2, fsnd/3, snd/2]).
:- use_module(library(plrand),      [log_partition_dirichlet/2]).

:- use_module(machines,   [unfold/2, unfolder/3, mapper/3, scan0/4, (:>)/3, mean/2, op(600,yfx,:>)]).
:- use_module(effects,    [dist/2, uniform/2]).
:- use_module(lazymath,   [map_sum/4]).
:- use_module(learn,      [converge/5, learn/4]).
:- use_module(switches,   [ map_sum_sw/3, map_sum_sw/4, map_swc/4
                          , sw_expectations/2, sw_log_prob/3, sw_posteriors/3, sw_samples/2
                          ]).
:- use_module(graph,      [ top_goal/1, top_value/2, tree_stats/2, sw_trees_stats/3, semiring_graph_fold/4
                          , graph_inside/3, graph_viterbi/4 , prune_graph/4, igraph_sample_tree/3
                          ]).

bernoulli(P1,X) :- P0 is 1-P1, dist([P0-0,P1-1],X).

mc_evidence(Method, Graph, Prior, Stream) :-
   converge(rel(1e-6), learn(vb(Prior), io(log), Graph), _, Prior, VBPost),
   sw_expectations(VBPost, VBProbs),
   call(top_value*semiring_graph_fold(r(log_e,lse,add,cons),Graph), VBProbs, LogPDataGivenVBProbs),
   call(add(LogPDataGivenVBProbs)*sw_log_prob(Prior), VBProbs, LogPDataVBProbs),
   method_machine_mapper(Method, Prior, Machine, Mapper),
   unfold(call(Machine, Graph, Prior, VBProbs)
          :> mapper(p_params_given_post(VBProbs)*Mapper) :> mean
          :> mapper(add(LogPDataVBProbs)*neg*log), Stream).

p_params_given_post(Probs,Post,P) :- sw_log_prob(Post,Probs,LP), P is exp(LP).

method_machine_mapper(gibbs,  _,     ccp_mcmc:gibbs_machine(posterior), =).
method_machine_mapper(mh,     Prior, ccp_mcmc:mh_machine, ccp_mcmc:sw_posteriors(Prior)*mcs_counts).

gibbs_machine(Rot, Graph, Prior, P1, M) :-
   graph_inside(Graph, P0, IG),
   rotation(Rot, sw_posteriors(Prior), gstep(P0,IG), sw_samples, Step),
   unfolder(scan0(Step), P1, M).

:- meta_predicate rotation(+,2,2,2,-).
rotation(posterior,Post, Step, Sample, Post*Step*Sample).
rotation(counts,   Post, Step, Sample, Step*Sample*Post).
rotation(params,   Post, Step, Sample, Sample*Post*Step).

gstep(P0,IG,P1,Counts) :-
   copy_term(P0-IG,P1-IG1),
   top_goal(Top),
   igraph_sample_tree(IG1, Top, Trees),
   tree_stats(Top-Trees, Counts).

mh_machine(Graph, Prior, Probs0, M) :-
   graph_as_conjunction(Graph, Graph1),
   graph_viterbi(Graph1, Probs0, VTrees, _),
   maplist(fst,Prior,SWs),
   mcs_init(SWs, VTrees, Keys, State),
   (  Keys=[] -> unfolder(scan0(=), State, M)
   ;  make_tree_sampler(Graph1, SampleGoal),
      (mcs_unit_counts(State) -> Stepper=gibbs; Stepper=mh),
      unfolder(scan0(mc_step(Stepper, Keys, SampleGoal, SWs, Prior)), State, M)
   ).

graph_as_conjunction(Graph, Graph) :- top_value(Graph, [_]), !.
graph_as_conjunction(Graph, [Top-[[Dummy]], Dummy-Expls | Graph0]) :-
   top_goal(Top), Dummy = '^mcmc':dummy,
   select(Top-Expls, Graph, Graph0).

mc_sample(SampleGoal, SWs, Probs, T1, T2) :-
   mct_goal(T1, Goal), call(SampleGoal, Probs, Goal, Tree),
   mct_make(SWs, Goal, Tree, T2).

make_tree_sampler(G, ccp_mcmc:sample_goal(IGs)) :-
   top_value(G, [Factors]),
   sort(Factors, UniqueFactors),
   maplist(sub_igraph(G), UniqueFactors, IGs).

sub_igraph(G, Goal, Goal-(IG-Ps)) :-
   prune_graph(=, Goal, G, SubGraph),
   graph_inside(SubGraph, Ps, IG).

sample_goal(IGs, PP, Goal, Trees) :-
   memberchk(Goal-(IG0-P0), IGs), % use rbtree for faster lookup
   copy_term(P0-IG0, P1-IG1),
   param_subset(P1, PP),
   igraph_sample_tree(IG1, Goal, Trees).

param_subset([], _).
param_subset([H1-V1|T1], [H2-V2|T2]) :-
    compare(Rel, H1, H2),
    psub_aux(Rel, H1, V1, V2, T1, T2).

psub_aux(>, H1, V1, _, T1, [H2-V2|T2]) :-
    compare(Rel, H1, H2),
    psub_aux(Rel, H1, V1, V2, T1, T2).
psub_aux(=, _, V, V, T1, T2) :-
    param_subset(T1, T2).

mc_step(gibbs, Keys, SampleGoal, SWs, Prior, State1, State2) :-
   mcs_random_select(Keys, TK_O, State1, StateExK),
   mcs_dcounts(StateExK, CountsExK),
   sw_posteriors(Prior, CountsExK, PostExK),
   sw_expectations(PostExK, ProbsExK),
   mc_sample(SampleGoal, SWs, ProbsExK, TK_O, TK_P),
   mcs_rebuild(TK_P, StateExK, State2).

mc_step(mh, Keys, SampleGoal, SWs, Prior, State1, State2) :-
   mcs_random_select(Keys, TK_O, State1, StateExK),
   mcs_dcounts(StateExK, CountsExK),
   sw_posteriors(Prior, CountsExK, PostExK),
   sw_expectations(PostExK, ProbsExK),
   mc_sample(SampleGoal, SWs, ProbsExK, TK_O, TK_P),
   maplist(tree_acceptance_weight(PostExK, ProbsExK), [TK_O, TK_P], [W_O, W_P]),
   D is W_P-W_O, (D>= -1e-13 -> Accept=1; call(bernoulli*exp, D, Accept)),
   (Accept=0 -> State2=State1; mcs_rebuild(TK_P, StateExK, State2)).

tree_acceptance_weight(PostExTree, PProbs, Tree, W) :-
   mct_counts(Tree, Counts),
   sw_posteriors(PostExTree, Counts, Post),
   map_sum_sw(log_partition_dirichlet, Post, LZ),
   map_sum_sw(map_sum(log_mul), PProbs, Counts, LP),
   W is LZ - LP.
log_mul(Prob, N, X) :- X is N*log(Prob).

% MCS: Monte Carlo state: rbtree to map K to tree, stash counts
mcs_init(SWs, VTrees, Ks, Totals-Map) :-
   sw_trees_stats(SWs, VTrees, Totals),
   call(list_to_rbtree * enumerate * map_stats(SWs) * include(is_pair), VTrees, Map),
   rb_keys(Map, Ks).

mcs_random_select(Ks, G-C, Totals-Map, dmhs(K,CountsExK,MapExK)) :-
   uniform(Ks,K),
   rb_delete(Map, K, G-C, MapExK),
   map_swc(sub, C, Totals, CountsExK).

mcs_rebuild(G-C, dmhs(K,CountsExK,MapExK), Totals-Map) :-
   sw_posteriors(C, CountsExK, Totals),
   rb_insert_new(MapExK, K, G-C, Map).

mcs_dcounts(dmhs(_,CountsExK,_), CountsExK).
mcs_counts(Counts-_, Counts).
mcs_unit_counts(_-Map) :-
   forall(rb_in(_,_-GCs,Map), forall(member(_-C, GCs), sumlist(C,1))).

mct_goal(Goal-_, Goal).
mct_make(SWs, Goal, T, Goal-C) :- sw_trees_stats(SWs,T,C).
mct_counts(_-C,C).

map_stats(SWs, Trees, Stats) :- maplist(fsnd(sw_trees_stats(SWs)), Trees, Stats).
