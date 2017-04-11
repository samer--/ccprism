:- module(autodiff, [mul/3, add/3, exp/2, log/2, deriv/3, go/0, clean/0]).

:- use_module(library(chr)).

:- chr_constraint deriv/3, mul/3, add/3, log/2, exp/2, go/0, clean/0.
:- chr_constraint agg/2, acc/1, acc/2, mul1/3, add1/3, log1/2, exp1/2.

% operations interface
mul(X,Y,Z) ==> mul1(X,Y,Z).
add(X,Y,Z) ==> add1(X,Y,Z).
log(X,Y)   ==> log1(X,Y).
exp(X,Y)   ==> exp1(X,Y).

% derivatives
deriv(L,X,DX) \ deriv(L,X,DX1) <=> DX=DX1.
deriv(L,L,DL) <=> DL=1.
deriv(_,_,DX) ==> acc(DX).
deriv(L,X,DX), log(X,Y)   ==> deriv(L,Y,DY), mul1(1/X,DY,Z), agg(Z,DX).
deriv(L,X,DX), exp(X,Y)   ==> deriv(L,Y,DY), mul1(Y,DY,Z),   agg(Z,DX).
deriv(L,X,DX), mul(K,X,Y) ==> deriv(L,Y,DY), mul1(K,DY,Z),   agg(Z,DX).
deriv(L,X,DX), mul(X,K,Y) ==> deriv(L,Y,DY), mul1(K,DY,Z),   agg(Z,DX).
deriv(L,X,DX), add(X,_,Y) ==> deriv(L,Y,DY),                 agg(DY,DX).
deriv(L,X,DX), add(_,X,Y) ==> deriv(L,Y,DY),                 agg(DY,DX).

% accumulation of sums
acc(X) \ acc(X) <=> true.

acc(X,S1), agg(Z,X) <=> add1(Z,S1,S2), acc(S2,X).
acc(X,S) <=> S=X.

% triggering accumulation
go \ acc(DX) <=> acc(DX,0).

% cleaning up...
clean \ deriv(_,_,_) <=> true.
clean \ add(_,_,_) <=> true.
clean \ mul(_,_,_) <=> true.
clean \ log(_,_) <=> true.
clean \ exp(_,_) <=> true.
clean \ go <=> true.
clean <=> true.

% low-level arithmetic
mul1(1,X,Y) <=> X=Y.
mul1(X,1,Y) <=> X=Y.
mul1(0,_,Y) <=> 0=Y.
mul1(_,0,Y) <=> 0=Y.
mul1(X,Y,Z) <=> ground(X-Y) | Z is X*Y.
add1(0,X,Y) <=> X=Y.
add1(X,0,Y) <=> X=Y.
add1(X,Y,Z) <=> ground(X-Y) | Z is X+Y.
log1(X,Y)   <=> ground(X) | Y is log(X).
exp1(X,Y)   <=> ground(X) | Y is exp(X).

