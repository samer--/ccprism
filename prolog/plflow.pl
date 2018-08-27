:- module(plflow, [ops_body/4]).

:- use_module(library(math), [stoch/3]).
:- use_module(library(plrand), []).

ops_body(_, _, Ops, Body) :- foldl(op_goal, Ops, Body, true).
op_goal(op(OpCode, Ins, Outs), (X,Y), Y) :- op_goal(OpCode, Ins, Outs, X).

:- multifile op_goal/4.
op_goal(add, [X,Y], [Z], Z is X + Y).
op_goal(sub, [X,Y], [Z], Z is X - Y).
op_goal(mul, [X,Y], [Z], Z is X * Y).
op_goal(div, [X,Y], [Z], Z is X / Y).
op_goal(pow, [X,Y], [Z], Z is Y**X).
op_goal(max, [X,Y], [Z], Z is max(X,Y)).
op_goal(exp, [X], [Z], Z is exp(X)).
op_goal(log, [X], [Z], Z is log(X)).
op_goal(chi, [X,Y,Z], [I], (X>Y -> I=Z; X<Y -> I=0.0; I is Z/2.0)).
op_goal(sum_list,   Xs, [Z], sum_list(Xs,Z)).
op_goal(max_list,   Xs, [Z], max_list(Xs,Z)).
op_goal(stoch, Xs, Ys, math:stoch(Xs,Ys,_)).
op_goal(log_prob_dirichlet(As),  Ps, [LP], plrand:log_prob_dirichlet(As,Ps,LP)).
op_goal(log_partition_dirichlet, As, [LZ], plrand:log_partition_dirichlet(As,LZ)).

