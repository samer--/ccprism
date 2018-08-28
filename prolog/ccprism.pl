:- module(ccprism, [goal_graph/2, graph_params/3, semiring_graph_fold/4, top_value/2]).

/** <module> Top level tabled explanation graph creation */

:- use_module(library(callutils), [(*)/4]).
:- use_module(library(rbutils),   [rb_fold/4, rb_add//2]).
:- use_module(ccprism/handlers,   [goal_expls_tables/3, tables_graph/2]).
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

%% graph_params(+Spec:sw_init_spec, +G:graph, -P:sw_params) is det.
%  Initialise parameters for all switches referenced in graph G.
%  See sw_init/2 for more information.
graph_params(Spec,G,Params) :- call(maplist(sw_init(Spec))*graph_switches, G, Params).
