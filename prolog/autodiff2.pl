:- module(autodiff2, [max/3, mul/3, add/3, pow/3, exp/2, llog/2, log/2, lse/2, deriv/3, back/1, grad/1,
                      esc/3, expand_wsums/0, wsum/2, add_to_wsum/3, gather_ops/1, topsort/4, topsort/5]).
/** <module> Reverse mode automatic differentatin using CHR.

 Todo:
 - consider sum operator
 - consider neg/sub/div operators
 - fix constant handling in deriv stoch_exp rule
*/

:- use_module(library(chr)).
:- use_module(library(rbutils)).
:- use_module(library(listutils), [measure/2]).
:- use_module(library(dcg_core), [seqmap//2]).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).

:- chr_constraint expand_wsums, wsum(?,-), add_to_wsum(?,?,-), ops(-,+).
:- chr_constraint max(?,?,-), add(?,?,-), mul(?,?,-), llog(-,-), log(-,-), exp(-,-), pow(+,-,-),
                  lse(?,-), stoch_exp(?,-), stoch_exp(?,+,-), mes(?,-,-,-), chi(?,?,?,-),
                  deriv(?,-,?), agg(?,-), acc(?,-), acc(-), go, esc(+,?,-).

add_to_wsum(X,0.0,S) <=> ord_list_to_rbtree([X-1], Terms), wsum(Terms, S).
add_to_wsum(X,S1,S2), wsum(Terms1, S1) <=> incr_term(X, Terms1, Terms2), wsum(Terms2, S2).

incr_term(X) --> rb_app_or_new(X, succ, =(1)).
add_mul(X-N, S1, S2) :- K is float(N), mul(K,X,NX), add(NX,S1,S2).
expand_wsums \ wsum(Terms, Sum) <=> rb_fold(add_mul, Terms, 0.0, Sum).
expand_wsums <=> true.

% operations interface with simplifications
mul(0.0,_,Y) <=> Y=0.0.
mul(_,0.0,Y) <=> Y=0.0.
mul(1.0,X,Y) <=> Y=X.
mul(X,1.0,Y) <=> Y=X.
mul(X,Y,Z1) \ mul(X,Y,Z2) <=> Z1=Z2.
pow(1,X,Y)   <=> Y=X.
pow(0,_,Y)   <=> Y=1.
add(0.0,X,Y) <=> Y=X.
add(X,0.0,Y) <=> Y=X.
add(X,Y,Z1) \ add(X,Y,Z2) <=> Z1=Z2.

% % collapse addions into products
% add(Y,X,XY), add(Y,XY,XYY) <=> mul(2,Y,YY), add(YY,X,XYY).
% add(YN,X,XYN), add(Y,XYN,XYNY), mul(N,Y,YN) <=> N1 is N+1, mul(N1,Y,YNY), add(YNY,X,XYNY).

% % collapse multiplications into powers
% mul(Y,X,XY), mul(Y,XY,XYY) <=> pow(2,Y,YY), mul(YY,X,XYY).
% mul(YN,X,XYN), mul(Y,XYN,XYNY), pow(N,Y,YN) <=> N1 is N+1, pow(N1,Y,YNY), mul(YNY,X,XYNY).

% lse: log(sum(map(exp,Xs))), stoch_exp: stoch(map(exp,Xs))
% mes: max, exp, sum - used to share computation of max(Xs), exp(Exs-Max) and sum
lse([X],Y) <=> X=Y.
lse(Xs,Y1) \ lse(Xs,Y2) <=> Y1=Y2.
lse(Xs,_) ==> mes(Xs,_,_,_).
stoch_exp(Xs,Ys1) \ stoch_exp(Xs,Ys2) <=> Ys1=Ys2.
stoch_exp(Xs,Ys) ==> mes(Xs,_,_,_), measure(Xs,Ns), maplist(stoch_exp(Xs),Ns,Ys).
mes(Xs,M1,Ws1,S1) \ mes(Xs,M2,Ws2,S2) <=> M1=M2, Ws1=Ws2, S1=S2.

% propagate derivatives through unary and binary operators
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,_,DX) <=> ground(L) | DX=0.0.
deriv(L,L,DL) ==> DL=1.0.
deriv(_,_,DX) ==> var(DX) | acc(DX).
deriv(L,Y,DY), pow(K,X,Y)   ==> deriv(L,X,DX), dpow(K,X,Z), mul(DY,Z,T), agg(T,DX).
deriv(L,Y,DY), exp(X,Y)     ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), llog(Y,X)    ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), log(X,Y)     ==> deriv(L,X,DX), pow(-1,X,RX), mul(RX,DY,T), agg(T,DX).
deriv(L,Y,DY), add(X1,X2,Y) ==> maplist(agg_add(L,DY),[X1,X2]).
deriv(L,Y,DY), mul(X1,X2,Y) ==> maplist(agg_mul(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), max(X1,X2,Y) ==> maplist(agg_max(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), lse(Xs,Y)    ==> stoch_exp(Xs,Ps), maplist(agg_mul(L,DY),Xs,Ps).
deriv(L,Y,DY), stoch_exp(Xs,N,Y) ==>
   pow(2,Y,Y2), mul(-1.0,Y2,NY2),
   mul(DY,NY2,T1), mul(DY,Y,T2),
   maplist(deriv(L),Xs,DXs), % !!! NB the rest is wrong for any constants in Xs
   maplist(agg(T1),DXs),
   nth1(N,DXs,DXN),
   agg(T2,DXN).

dpow(K,X,T) :- K1 is K - 1, KK is float(K), pow(K1,X,XpowK1), mul(KK,XpowK1,T).
agg_max(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), chi(X1,X2,DY,T1), agg(T1,DX1); true.
agg_mul(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), mul(X2,DY,T1), agg(T1,DX1); true.
agg_add(L,DY,X1)    :- var(X1) -> deriv(L,X1,DX1), agg(DY,DX1); true.
acc(X) \ acc(X) <=> true.

% initiatiate back-propagation starting from Y
back(Y) :- var(Y) -> diff(Y), go; true.
diff(Y) :- deriv(Y,Y,1.0).
grad(Ys) :- maplist(diff,Ys), go.

acc(X,S1), agg(Z,X) <=> add(Z,S1,S2), acc(X,S2).
acc(X,S) <=> S=X.

go \ deriv(_,_,_) <=> true.
go \ acc(DX) <=> acc(DX,0.0).
go <=> true.

gather_ops(G) :- ops(G,[]).

:- meta_predicate upd_ops(//,?,?).
upd_ops(Upd,G1,G3) :- call(Upd,G1,G2), ops(G2,G3).
op(Op, Ins, Outs) --> [op(Op,Ins,Outs)].

% these don't make much difference..
% goal_expansion(op(Op,Ins,Outs,G1,G2), G1 = [op(Op,Ins,Outs)|G2]).
% goal_expansion(upd_ops(P,G1,G3), (call(P,G1,G2), ops(G2,G3))).

ops(G1,G2), add(X,Y,Z) <=> upd_ops(op(add, [X,Y], [Z]), G1, G2).
ops(G1,G2), mul(X,Y,Z) <=> upd_ops(op(mul, [X,Y], [Z]), G1, G2).
ops(G1,G2), max(X,Y,Z) <=> upd_ops(op(max, [X,Y], [Z]), G1, G2).
ops(G1,G2), pow(X,Y,Z) <=> upd_ops(op(pow, [X,Y], [Z]), G1, G2).
ops(G1,G2), log(X,Y)   <=> upd_ops(op(log, [X], [Y]), G1, G2).
ops(G1,G2), exp(X,Y)   <=> upd_ops(op(exp, [X], [Y]), G1, G2).
ops(G1,G2), esc(Op,X,Y)<=> upd_ops(op(Op, X, Y), G1, G2).
ops(_,_) \ llog(_,_)   <=> true.

ops(_,_) \ stoch_exp(_,_,_)  <=> true.
mes(Xs,M,_,S)  \ ops(G1,G2), lse(Xs,Y)        <=> mes(Xs,M,_,S), upd_ops(add_log(S,M,Y), G1, G2).
mes(Xs,_,Ws,S) \ ops(G1,G2), stoch_exp(Xs,Ys) <=> upd_ops(divby_list(S,Ws,Ys), G1, G2).
ops(G1,G2), mes(Xs,M,Ws,S)                    <=> upd_ops(max_exp_sum(Xs,M,Ws,S),G1,G2).
ops(G1,G2), chi(X,Y,Z,I)                      <=> upd_ops(op(chi, [X,Y,Z], [I]), G1, G2).
ops(G1,G2) <=> G1=G2.

add_log(S,M,Y) --> op(log,[S],[LogS]), op(add,[LogS,M],[Y]).
divby_list(S,Ws,Ys) --> foldl(divby(S), Ws, Ys).
divby(S,W,Y) --> op(div, [W,S], [Y]).

max_exp_sum(Xs,M,Ws,Sum) -->
   op(max_list, Xs, [M]),
   foldl(exp_sub(M),Xs,Ws),
   op(sum_list, Ws, [Sum]).
exp_sub(M,X,Y) --> op(sub, [X,M], [XsubM]), op(exp, [XsubM], [Y]).

topsort(Ins, Outs, Ops, Sorted) :- topsort(Ins, Outs, Ops, Sorted, []).
topsort(Ins, Outs, Ops, S1, S2) :-
   rb_empty(E),
   seqmap(back_links, Ops, E, BS),
   traverse(BS, Ins, Outs, S1-E, S2-_).

back_links(Edge) --> {Edge=op(_,_,Outs)}, seqmap(back_link(Edge), Outs).
back_link(Edge, Out) --> rb_add(Out, Edge).
traverse(BS, Ins, Outs) --> \> seqmap(insert, Ins), seqmap(eval(BS), Outs).
insert(X) --> rb_add(X,t).

eval(BS, Var) -->
   (  ({nonvar(Var)}; \> rb_get(Var, _)) -> []
   ;  {rb_lookup(Var, Edge, BS), Edge=op(_,Ins,Outs)},
      seqmap(eval(BS), Ins),
      [Edge] <\> seqmap(insert, Outs)
   ).
