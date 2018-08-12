:- module(dice, [dice/2, die//1]).

:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).

die +-> [1,2,3,4].

:- cctable dice/2.
dice(0,0).
dice(N,Z) :- succ(M,N), die := X, dice(M,Y), Z is X+Y.
