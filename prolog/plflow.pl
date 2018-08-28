:- module(plflow, [ops_body/4, sub/3, stoch/2, log_prob_dir/3, log_part_dir/2, mean_log_dir/2]).

:- use_module(library(math), [stoch/3]).
:- use_module(library(plrand), []).

ops_body(_, _, Ops, Body) :- foldl(op_goal, Ops, Body, true).
op_goal(op(OpCode, Ins, Outs), (X,Y), Y) :- op_goal(OpCode, Ins, Outs, X).

:- multifile op_goal/4.
op_goal(add, [X,Y], [Z], Z is X + Y).
op_goal(sub, [X,Y], [Z], Z is Y - X).
op_goal(mul, [X,Y], [Z], Z is X * Y).
op_goal(div, [X,Y], [Z], Z is X / Y).
op_goal(pow, [X,Y], [Z], Z is Y**X).
op_goal(max, [X,Y], [Z], Z is max(X,Y)).
op_goal(exp, [X], [Z], Z is exp(X)).
op_goal(log, [X], [Z], Z is log(X)).
op_goal(chi, [X,Y,Z], [I], (X>Y -> I=Z; X<Y -> I=0.0; I is Z/2.0)).
op_goal(add_log, [M,S], [Z], Z is log(S) + M).
op_goal(exp_sub, [M,Z], [S], S is exp(Z-M)).
op_goal(sum_list,   Xs, [Z], sum_list(Xs,Z)).
op_goal(max_list,   Xs, [Z], max_list(Xs,Z)).
op_goal(stoch, Xs, Ys, math:stoch(Xs,Ys,_)).
op_goal(log_prob_dirichlet(As),  Ps, [LP], plrand:log_prob_dirichlet(As,Ps,LP)).
op_goal(log_partition_dirichlet, As, [LZ], plrand:log_partition_dirichlet(As,LZ)).
op_goal(mean_log_dirichlet, As, Psi, plrand:mean_log_dirichlet(As,Psi)).

log_prob_dir(As, Ps, LP) :- esc(log_prob_dirichlet(As), Ps, [LP]).
log_part_dir(As, LZ)     :- esc(log_partition_dirichlet, As, [LZ]).
mean_log_dir(As, Psi)    :- same_length(As, Psi), esc(mean_log_dirichlet, As, Psi).
stoch(Xs,Ys) :- same_length(Xs,Ys), esc(stoch,Xs,Ys).
sub(X,Y,Z) :- esc(sub, [X,Y], [Z]).
