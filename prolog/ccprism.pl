:- module(ccprism, [ goal_graph/2, goal_prob/3, graph_params/3]).

/** <module> Top level tabled explanation graph creation */

:- use_module(library(callutils), [(*)/4]).
:- use_module(library(rbutils),   [rb_fold/4, rb_add//2]).
:- use_module(ccprism/handlers,   [goal_expls_tables/3]).
:- use_module(ccprism/graph,      [prune_graph/4, graph_switches/2, semiring_graph_fold/4, top_value/2]).
:- use_module(ccprism/switches,   [sw_init/3]).

%% goal_graph(+Goal:callable, -Graph:graph) is det.
%  Finds all solutions to Goal in a delimited context supplying tabling and 
%  probabilistic choice. Explanations are extracted from the tables and 
%  returned as a hypergraph, including explanations of Goal itself (which need
%  not be tabled) under the pseudo-goal =|'^top':top|=.
:- meta_predicate goal_graph(0,-).
goal_graph(Goal, Graph) :- 
   time(goal_expls_tables(Goal, Es, Tables)),
   tables_graph(Tables, Graph0),
   prune_graph(=, '^top':top, [('^top':top)-Es|Graph0], Graph).

tables_graph(Tables, Graph) :-
   rb_empty(Empty),
   foldl(goal_expls, Tables, Empty, GMap),
   rb_visit(GMap, Graph).

goal_expls(Goal-Solns) -->
   {term_variables(Goal,Vars)},
   foldl(soln_expls(Goal,Vars), Solns).

soln_expls(G,Y,Y1-Es) -->
   {copy_term(G-Y,G1-Y1), numbervars(G1-Y1, 0, _)}, % NB Es is already ground
   (rb_add(G1,Es) -> []; []). % NB duplicate goals should have the same explanations!

%% graph_params(+Spec:sw_init_spec, +G:graph, -P:sw_params) is det.
%  Initialise parameters for all switches referenced in graph G.
%  See sw_init/2 for more information.
graph_params(Spec,G,Params) :- call(maplist(sw_init(Spec))*graph_switches, G, Params).

:- meta_predicate goal_prob(0,+,-).
goal_prob(Goal,ParamSpec,Prob) :-
   goal_graph(Goal, Graph),
   graph_params(ParamSpec, Graph, Params),
   semiring_graph_fold(r(=,=,mul,add), Graph, Params, IG),
   top_value(IG, Prob).
