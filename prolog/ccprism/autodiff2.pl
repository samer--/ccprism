:- module(ccp_autodiff2, [graph_counts_ad/5, graph_counts_ad/6]).

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
graph_counts_ad(ISc, PSc, Graph, Params, Eta, LogProb) :- 
   scaling_semiring(ISc, SR, ToLogProb),
   semiring_graph_fold(SR, Graph, P0, IG),
   call(ToLogProb*top_value, IG, LogProb),
   scaling_log_params(ISc, PSc, P0, Params0, LogP0),
   map_swc(deriv(LogProb), LogP0, Eta),
   back(LogProb), compile, Params=Params0.

scaling_semiring(lin, r(=,=,autodiff2:mul,autodiff2:add), log).
scaling_semiring(log, r(=,autodiff2:lse,autodiff2:add,cons), =).

scaling_log_params(lin, lin, P0,    P0,    LogP0) :- map_swc(llog, P0, LogP0).
scaling_log_params(lin, log, P0,    LogP0, LogP0) :- map_swc(exp, LogP0, P0).
scaling_log_params(log, lin, LogP0, P0,    LogP0) :- map_swc(log, P0, LogP0).
scaling_log_params(log, log, LogP0, LogP0, LogP0). 

