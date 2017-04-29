:- module(autodiff2, [max/3, mul/3, add/3, pow/3, exp/2, llog/2, log/2, lse/2, deriv/3, back/1, compile/0]).
/** <module> Reverse mode automatic differentatin using CHR.
 
 Todo: 
 - check ground constants are handled correctly in places where fixed probabilities might occur.
 - check performance impact of complete stoch_exp etc handling
 - check performance impact of powers vs. multiplications or divisions
*/

:- use_module(library(chr)).

:- chr_constraint max(?,?,-), add(?,?,-), mul(?,?,-), llog(-,-), log(-,-), exp(-,-), pow(+,-,-),
                  lse(?,-), stoch_exp(?,-), stoch_exp(?,+,-), mx_e_s(+,-,-,-),
                  deriv(?,-,?), agg(?,-), acc(?,-), acc(-), go, compile.

% operations interface with simplifications
mul(0.0,_,Y) <=> Y=0.0.
mul(_,0.0,Y) <=> Y=0.0.
mul(1.0,X,Y) <=> Y=X.
mul(X,1.0,Y) <=> Y=X.
mul(X,Y,Z1) \ mul(X,Y,Z2) <=> Z1=Z2.
pow(1,X,Y) <=> Y=X.
pow(0,_,Y) <=> Y=1.
add(0.0,X,Y) <=> Y=X.
add(X,0.0,Y) <=> Y=X.
add(X,Y,Z1) \ add(X,Y,Z2) <=> Z1=Z2.

% lse: log(sum(map(exp,Xs))), stoch_exp: stoch(map(exp,Xs))
% Note: machinery for 2nd derivatives of lse is here but commented out for speed.
lse([X],Y) <=> X=Y. 
lse(Xs,Y1) \ lse(Xs,Y2) <=> Y1=Y2.
lse(Xs,_) ==> mx_e_s(Xs,_,_,_).
% stoch_exp(Xs,Ys1) \ stoch_exp(Xs,Ys2) <=> Ys1=Ys2.
stoch_exp(Xs,Ys) ==> same_length(Xs,Ys).
% stoch_exp(Xs,Ys) ==> 
%    mx_e_s(Xs,_,_,_),
%    length(Xs,N), numlist(1,N,Ns), 
%    maplist(stoch_exp(Xs),Ns,Ys).
% mx_e_s(Xs,M1,Ws1,S1) \ mx_e_s(Xs,M2,Ws2,S2) <=> M1=M2, Ws1=Ws2, S1=S2.

% propagate derivatives through unary and binary operators
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,_,DX) <=> ground(L) | DX=0.0.
deriv(_,_,DX) ==> var(DX) | acc(DX).
deriv(L,Y,DY), pow(K,X,Y)   ==> deriv(L,X,DX), dpow(K,X,Z), mul(DY,Z,T), agg(T,DX).
deriv(L,Y,DY), exp(X,Y)     ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), llog(Y,X)    ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), log(X,Y)     ==> deriv(L,X,DX), pow(-1,X,RX), mul(RX,DY,T), agg(T,DX).
deriv(L,Y,DY), add(X1,X2,Y) ==> maplist(agg_add(L,DY),[X1,X2]).
deriv(L,Y,DY), mul(X1,X2,Y) ==> maplist(agg_mul(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), max(X1,X2,Y) ==> maplist(agg_max(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), lse(Xs,Y)    ==> stoch_exp(Xs,Ps), maplist(agg_mul(L,DY),Xs,Ps).
% deriv(L,Y,DY), stoch_exp(Xs,N,Y) ==>
%    pow(2,Y,Y2), mul(-1.0,Y2,NY2), 
%    mul(DY,NY2,T1), mul(DY,Y,T2),
%    maplist(deriv(L),Xs,DXs),
%    maplist(agg(T1),DXs),
%    nth1(N,DXs,DXN), 
%    agg(T2,DXN). 

dpow(K,X,T) :- K1 is K - 1, KK is float(K), pow(K1,X,XpowK1), mul(KK,XpowK1,T).
agg_max(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), ifge(X1,X2,DY,T1), agg(T1,DX1); true.
agg_mul(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), mul(X2,DY,T1), agg(T1,DX1); true.
agg_add(L,DY,X1)    :- var(X1) -> deriv(L,X1,DX1), agg(DY,DX1); true.
acc(X) \ acc(X) <=> true.

% initiatiate back-propagation starting from Y
back(Y) :- var(Y) -> deriv(Y,Y,1.0), go; true.

acc(X,S1), agg(Z,X) <=> add(Z,S1,S2), acc(X,S2).
acc(X,S) <=> S=X.

go \ deriv(_,_,_) <=> true.
go \ acc(DX) <=> acc(DX,0.0).
go <=> true.

% convert arithmetic constraints to frozen goals.
compile \ max(X,Y,Z) <=> delay(max(X,Y),Z).
compile \ add(X,Y,Z) <=> delay(X+Y,Z).
compile \ mul(X,Y,Z) <=> delay(X*Y,Z).
compile \ add(X,Y,Z) <=> delay(X+Y,Z).
compile \ log(X,Y)   <=> delay(log(X),Y).
compile \ exp(X,Y)   <=> delay(exp(X),Y).
compile \ pow(K,X,Y) <=> delay(X**K,Y).
compile \ llog(_,_)  <=> true.

% compile \ stoch_exp(_,_,_) <=> true.
compile, mx_e_s(Xs,M,_,S)  \ lse(Xs,Y)        <=> when(ground(S), Y is M+log(S)).
compile, mx_e_s(Xs,_,Ws,S) \ stoch_exp(Xs,Ys) <=> when(ground(S), maplist(divby(S),Ws,Ys)).
compile\ mx_e_s(Xs,M,Ws,S)                    <=> when(ground(Xs), max_exp_sum(Xs,M,Ws,S)).

compile <=> true.

delay(Expr,Res) :- when(ground(Expr), Res is Expr).
ifge(X,Y,Z,I)  :- when(ground(X-Y), (X>=Y -> I=Z; I=0.0)).
divby(K,X,Y) :- Y is X/K.

max_exp_sum(Xs,M,Ws,Sum) :-
   max_list(Xs,M),
   maplist(exp_sub(M),Xs,Ws),
   sum_list(Ws, Sum).
exp_sub(M,X,Y) :- Y is exp(X-M).

