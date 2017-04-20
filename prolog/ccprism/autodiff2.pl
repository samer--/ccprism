:- module(ccp_autodiff2, [graph_counts_ad/5]).

/** <module> Inside-Outside computation using automatic differentiation (variant) */

:- use_module(library(callutils), [(*)/4]).
:- use_module(library(autodiff2),  [llog/2, log/2, exp/2, add/3, mul/3, back/1, deriv/3, compile/0]).
:- use_module(graph,    [top_value/2, semiring_graph_fold/4]).
:- use_module(switches, [map_swc/3, map_swc/4]).

ccp_graph:m_zero(autodiff2:mul,1.0).
ccp_graph:m_zero(autodiff2:add,0.0).

%% graph_counts_ad(+Scaling:scaling, +G:graph, P:sw_params, C:sw_params, LP:number) is det.
%
%  Compute expected switch counts C from explanation graph G with switch parameters
%  P. Uses automatic differentiation of the expression for the log of the inside 
%  probability LP of the graph. Params can be unbound - binding them later triggers
%  the computations required to yield numerical values in the result.
graph_counts_ad(Sc, Graph, Params, Eta, LogProb) :- 
   semiring_graph_fold(r(=,=,autodiff2:mul,autodiff2:add), Graph, P0, IG),
   call(log*top_value, IG, LogProb),
   grad_log_params(Sc, LogProb, P0, Eta, Params0),
   back(LogProb), compile, Params=Params0.

grad_log_params(lin, LogProb, P0, Eta, P0) :- 
   map_swc(deriv(LogProb)*llog, P0, Eta).
grad_log_params(log, LogProb, P0, Eta, LogP0) :-
   map_swc(exp, LogP0, P0),
   map_swc(deriv(LogProb), LogP0, Eta).