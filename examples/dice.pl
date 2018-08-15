:- module(dice, [two_dice/1, two_dice/2, three_dice/1, dice/2, die/3]).

:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).

die +-> [1,2,3,4].
die(_) +-> [1,2,3]. % impossible three sided die

:- cctable dice/2.
dice(0,0).
dice(N,Z) :- succ(M,N), die := X, dice(M,Y), Z is X+Y.

:- cctable three_dice/1, two_dice/2, two_dice/1.
three_dice(X)   :- length(Xs,3), maplist(:=(die), Xs), sumlist(Xs,X).
two_dice(X1,X2) :- die := X1, die := X2.
two_dice(X)     :- die(1) := D1, die(2) := D2, X is D1+D2.

% :- use_module(library(clpfd)).
% dice(N,Z) :-
%    die := X,
%    nonneg(Y), Z #= X+Y,
%    nonneg(M), N #= M+1,
%    dice(M,Y).
% nonneg(X) :- X #>= 0.
