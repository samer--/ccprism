:- module(ccp_sr_autodiff, [graph_io_auto/5]).

:- use_module(library(dcg_pair)).
:- use_module(library(listutils), [foldr/4]).
:- use_module(graph,    [top_value/2, semiring_graph_fold/4]).
:- use_module(switches, [map_swc/3]).
:- use_module(lazymath, []).
:- use_module(autodiff, [log/2, add/3, mul/3, go/0, clean/0, deriv/3]).

ccp_graph:sr_inj(auto(_,_),   _, P, P).
ccp_graph:sr_proj(auto(_,_),  _, X, X, X).
ccp_graph:sr_plus(auto(_,_),  X) --> add(X).
ccp_graph:sr_times(auto(_,_), X) --> mul(X).
ccp_graph:sr_zero(auto(_,Z),  Z).
ccp_graph:sr_unit(auto(O,_),  O).

graph_io_auto(Graph, Params, IGraph, LogProb, Eta)  :- 
   semiring_graph_fold(auto(O,Z), Graph, P0, IGraph),
   top_value(IGraph, PInside),
   log(PInside, LogProb),
   map_swc(deriv(LogProb), P0, Eta),
   go, clean, O=1, Z=0, Params=P0.
