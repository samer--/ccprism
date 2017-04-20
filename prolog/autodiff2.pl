:- module(autodiff2, [max/3, mul/3, add/3, pow/3, exp/2, llog/2, log/2, deriv/3, back/1, compile/0]).

:- use_module(library(chr)).

:- chr_constraint max(?,?,-), add(?,?,-), mul(?,?,-), llog(-,-), log(-,-), exp(-,-), pow(+,-,-),
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

% propagate derivatives through unary and binary operators
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,_,DX) <=> ground(L) | DX=0.0.
deriv(_,_,DX) ==> var(DX) | acc(DX).
deriv(L,Y,DY), pow(K,X,Y)   ==> deriv(L,X,DX), pow_contrib(K,X,Z), mul(DY,Z,T), agg(T,DX).
deriv(L,Y,DY), exp(X,Y)     ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), llog(Y,X)    ==> deriv(L,X,DX), mul(Y,DY,T), agg(T,DX).
deriv(L,Y,DY), log(X,Y)     ==> deriv(L,X,DX), pow(-1,X,RX), mul(RX,DY,T), agg(T,DX).
deriv(L,Y,DY), add(X1,X2,Y) ==> maplist(agg_add_contrib(L,DY),[X1,X2]).
deriv(L,Y,DY), mul(X1,X2,Y) ==> maplist(agg_mul_contrib(L,DY),[X1,X2],[X2,X1]).
deriv(L,Y,DY), max(X1,X2,Y) ==> maplist(agg_max_contrib(L,DY),[X1,X2],[X2,X1]).

pow_contrib(K,X,T) :- K1 is K - 1, KK is float(K), pow(K1,X,XpowK1), mul(KK,XpowK1,T).
agg_max_contrib(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), ifge(X1,X2,DY,T1), agg(T1,DX1); true.
agg_mul_contrib(L,DY,X1,X2) :- var(X1) -> deriv(L,X1,DX1), mul(X2,DY,T1), agg(T1,DX1); true.
agg_add_contrib(L,DY,X1)    :- var(X1) -> deriv(L,X1,DX1), agg(DY,DX1); true.

acc(X) \ acc(X) <=> true.

% initiatiate back-propagation starting from Y
back(Y) :- var(Y) -> deriv(Y,Y,1.0), go; true.

acc(S1,X), agg(Z,X) <=> add(Z,S1,S2), acc(S2,X).
acc(S,X) <=> S=X.

go \ deriv(_,_,_) <=> true.
go \ acc(DX) <=> acc(0.0,DX).
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
compile <=> true.

delay(Expr,Res) :- when(ground(Expr), Res is Expr).
ifge(X,Y,Z,I)  :- when(ground(X-Y), (X>=Y -> I=Z; I=0.0)).
