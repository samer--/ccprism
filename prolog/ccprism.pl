:- module(ccprism, [ goal_graph/2, tables_graph/2]).

/** <module> Top level runner */

:- use_module(library(rbutils),     [rb_fold/4, rb_add//2]).
:- use_module(ccprism/handlers,     [run_with_tables/2, run_prob/4, run_tab/2, expl//1]).
:- use_module(ccprism/graph,        [prune_graph/4]).

:- meta_predicate goal_graph(0,-).
goal_graph(Goal, Graph) :- 
   time(run_with_tables(run_tab(findall(E,run_prob(expl,Goal,E,[]),Es), Es), Tables)),
   tables_graph(Tables, Graph0),
   prune_graph(=, top:'$top$', [(top:'$top$')-Es|Graph0], Graph).

tables_graph(Tables, Graph) :-
   rb_empty(Empty),
   rb_fold(goal_expls, Tables, Empty, GMap),
   rb_visit(GMap, Graph).

goal_expls(_-tab(Goal,Solns,_)) -->
   {term_variables(Goal,Vars)},
   rb_fold(soln_expls(Goal,Vars), Solns).
soln_expls(G,Y,Y1-Es) -->
   {copy_term(G-Y,G1-Y1), numbervars(G1-Y1, 0, _)}, % NB Es is already ground
   (rb_add(G1,Es) -> []; []). % NB duplicate goals should have the same explanations!

