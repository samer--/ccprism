:- module(juliaflow, [ops_body/4]).

:- use_module(library(julia)).
:- use_module(library(insist), [insist/1]).

:- initialization(init).

init :-
   !using("Distributions"),
   !lmvbeta = ([X::'Array{Float64}'] \\ sum('lgamma.'(X)) - lgamma(sum(X))),
   !stoch   = ([X::'Array{Float64}'] \\ X/sum(X)).

op_to_jl(op(OpCode, Ins, Outs), Expr) -->
   ( op(OpCode, Ins, Outs, Expr) -> []
   ; {op(OpCode, Ins, Outs, Expr)},
     foldl(name_output, Outs)
   ).

op(add, [X,Y], [Z], Z = X + Y).
op(sub, [X,Y], [Z], Z = X - Y).
op(mul, [X,Y], [Z], Z = X * Y).
op(div, [X,Y], [Z], Z = X / Y).
op(pow, [X,Y], [Z], Z = Y^X).
op(max, [X,Y], [Z], Z = max(X,Y)).
op(log, [X], [Z], Z = log(X)).
op(exp, [X], [Z], Z = exp(X)).
op(chi, [X,Y,Z], [I], I = if(X>Y, Z, if(X<Y, 0.0, Z/2.0))).
op(sum_list, Xs, [Z], Z = sum(Xs)).
op(max_list, Xs, [Z], Z = maximum(Xs)).
op(log_partition_dirichlet, As, [LZ], LZ = lmvbeta(As)).
op(log_prob_dirichlet(As),  Ps, [LP], LP = pdf(DirAs, float64(_,Ps))) :-
   DirAs ?? 'Dirichlet'(float64(_,As)).

op(add_log, [S,M], [Y], Y = M + log(S)).

op(stoch, Xs, Ys,  Vec = stoch(Xs)) -->
   name_output(Vec), {foldl(name_component(Vec), Ys, 0, _)}.
% op(divby_list, [S|Xs], Ys,  Vec = Xs./S) -->
%    name_output(Vec), {foldl(name_component(Vec), Ys, 0, _)}.
% op(exp_sub, [M|Xs], Ys,  Vec = 'exp.'(Xs - M)) -->
%    name_output(Vec), {foldl(name_component(Vec), Ys, 0, _)}.

name_component(X, X[J], I, J) :- succ(I,J).
name_output(N, I, J) :- succ(I,J), atom_concat('ξ',J,N).
seq_jl(X, X:>:Y, Y).

ops_body(Ins, Outs, Ops, (Fn=_, jl_call(FnName, float64([Nin], Ins), float64(_, Outs)))) :-
   copy_term(t(Ins,Outs,Ops), t(Ins1,Outs1,Ops1)),
   length(Ins, Nin),
   foldl(name_component(x), Ins1, 0, _),
   foldl(op_to_jl, Ops1, Exprs, 0, _),
   foldl(seq_jl, Exprs, Body, float64(_, Outs1)),
   $Fn ?? ([x::'Array{Float64}']\\Body),
   julia:jl_ws_name(Fn, FnName).
