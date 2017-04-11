:- module(ccp_sr_autodiff, [graph_counts_ad/5]).

:- use_module(library(callutils), [(*)/4]).
:- use_module(graph,    [top_value/2, semiring_graph_fold/4]).
:- use_module(autodiff, [llog/2, log/2, exp/2, add/3, mul/3, max/3, go/0, clean/0, deriv/3]).
:- use_module(switches, [map_swc/3, map_swc/4]).
:- use_module(lazymath, []).

ccp_graph:sr_inj(auto(_,_),   _, P, P).
ccp_graph:sr_proj(auto(_,_),  _, X, X, X).
ccp_graph:sr_plus(auto(_,_),  X) --> add(X).
ccp_graph:sr_plus(auto(_,_),  X) --> max(X).
ccp_graph:sr_times(auto(_,_), X) --> mul(X).
ccp_graph:sr_zero(auto(_,Z),  Z).
ccp_graph:sr_unit(auto(O,_),  O).

graph_counts_ad(Me, Sc, Graph, Params, LogProb-Eta) :- 
   call(log*top_value*semiring_graph_fold(auto(O,Z), Graph), P0, LogProb),
   grad_log_params(Me/Sc, LogProb, P0, Eta, Params0),
   go, clean, O=1, Z=0, 
   Params=Params0.

grad_log_params(i/lin, LogProb, P0, Eta, P0) :-
   map_swc(deriv(LogProb), P0, Grad),
   map_swc(lazymath:mul, P0, Grad, Eta).
grad_log_params(i2/lin, LogProb, P0, Eta, P0) :- 
   map_swc(deriv(LogProb)*llog, P0, Eta).
grad_log_params(i2/log, LogProb, Eta, P0, LogP0) :-
   map_swc(exp, LogP0, P0),
   map_swc(deriv(LogProb), LogP0, Eta).

