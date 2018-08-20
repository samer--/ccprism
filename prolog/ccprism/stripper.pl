:- module(stripper, [strip_graph_modules/2]).
/* Generalised semiring to strip module qualification from switches and
*  goals in a graph. This renders the graph useless for computation but
*  can be useful for printing and visualisations where redundant module
*  qualifiers would only add visual clutter.
*/
:- use_module(library(listutils),   [cons//1]).
:- use_module(library(data/pair),   [ffst/3]).
:- use_module(graph, [semiring_graph_fold/4]).

strip_graph_modules(G1, G3) :-
   semiring_graph_fold(strip, G1, _, G2),
   maplist(ffst(strip), G2, G3).

strip(_:F, F).

ccp_graph:sr_inj(strip, _, (_:SW):=V, SW:=V).
ccp_graph:sr_proj(strip, _:G, Z, G, Z).
ccp_graph:sr_plus(strip, Expl) --> cons(Expl).
ccp_graph:sr_times(strip, F)   --> cons(F).
ccp_graph:sr_zero(strip, []).
ccp_graph:sr_unit(strip, []).
