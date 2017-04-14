:- module(autodiff, [mul/3, max/3, add/3, exp/2, llog/2, log/2, deriv/3, go/0, clean/0]).

:- use_module(library(chr)).

:- chr_constraint max(?,?,-), add(?,?,-), mul(?,?,-), llog(-,-), log(-,-), exp(-,-),
                  deriv(-,-,?), agg(?,-), acc(?,-), acc(-), go, clean.

% operations interface
mul(1.0,X,Y) <=> Y=X.
mul(X,1.0,Y) <=> Y=X.
mul(X,Y,Z) ==> delay(X*Y,Z).
add(0.0,X,Y) <=> Y=X.
add(X,0.0,Y) <=> Y=X.
add(X,Y,Z) ==> delay(X+Y,Z).
max(X,Y,Z) ==> delay(max(X,Y),Z).
log(X,Y)   ==> delay(log(X),Y).
exp(X,Y)   ==> delay(exp(X),Y).

% derivatives
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,L,DL) <=> DL=1.0.
deriv(_,_,DX) ==> var(DX) | acc(DX).
deriv(L,X,DX), log(X,Y)   ==> deriv(L,Y,DY), delay(DY/X,Z),  agg(Z,DX).
deriv(L,X,DX), llog(Y,X)  ==> deriv(L,Y,DY), delay(Y*DY,Z),  agg(Z,DX).
deriv(L,X,DX), exp(X,Y)   ==> deriv(L,Y,DY), delay(Y*DY,Z),  agg(Z,DX).
deriv(L,X,DX), mul(K,X,Y) ==> deriv(L,Y,DY), delay(K*DY,Z),  agg(Z,DX).
deriv(L,X,DX), mul(X,K,Y) ==> deriv(L,Y,DY), delay(K*DY,Z),  agg(Z,DX).
deriv(L,X,DX), max(X,Y,Z) ==> deriv(L,Z,DZ), ifge(X,Y,DZ,W), agg(W,DX).
deriv(L,X,DX), max(Y,X,Z) ==> deriv(L,Z,DZ), ifge(X,Y,DZ,W), agg(W,DX).
deriv(L,X,DX), add(X,_,Y) ==> deriv(L,Y,DY),                 agg(DY,DX).
deriv(L,X,DX), add(_,X,Y) ==> deriv(L,Y,DY),                 agg(DY,DX).

% accumulation of sums
acc(X) \ acc(X) <=> true.

acc(S1,X), agg(Z,X) <=> delay(Z+S1,S2), acc(S2,X).
acc(S,X) <=> S=X.

% triggering accumulation
go \ acc(DX) <=> acc(0.0,DX).

% cleaning up...
clean \ deriv(_,_,_) <=> true.
clean \ add(_,_,_) <=> true.
clean \ max(_,_,_) <=> true.
clean \ mul(_,_,_) <=> true.
clean \ log(_,_) <=> true.
clean \ llog(_,_) <=> true.
clean \ exp(_,_) <=> true.
clean \ go <=> true.
clean <=> true.

delay(Expr,Res) :- when(ground(Expr), Res is Expr).
ifge(X,Y,Z,I)  :- when(ground(X-Y), (X>=Y -> I=Z; I=0.0)).
