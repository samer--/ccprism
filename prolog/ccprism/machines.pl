:- module(machines, [ iterate/4, unfold/2, unfold_finite/2, foreach/2, cofold/3, scanner/3, scan0/4, drop/3, mean/2, mean/5
                    , (>>)/4, (:>)/3, iterator/3, unfolder/3, mapper/3, moore/5, subsample/3
                    , op(600,yfx,:>), op(500,xfy,>>)
                    ]).

:- use_module(library(dcg_progress), [seqmap_with_progress//3]).
:- use_module(library(math),         [add/3, divby/3]).
:- use_module(library(lazy),         [lazy_unfold/4, lazy_unfold_finite/4]).

% running machines
:- meta_predicate unfold(1,-), unfold_finite(1,-), iterate(1,?,+,-), foreach(1,1).
unfold(MakeMachine, Stream) :- call(MakeMachine,unfolder(T,S)), lazy_unfold(T,Stream,S,_).
unfold_finite(MakeMachine, Stream) :- call(MakeMachine,unfolder(T,S)), lazy_unfold_finite(T,Stream,S,_).
iterate(Setup, LPs) --> {call(Setup, Step)}, seqmap_with_progress(1,Step,LPs).

foreach(P, MakeMachine) :- call(MakeMachine, unfolder(T,S)), foreach_(P,T,S).
foreach_(P,T,S1) :- call(T,X,S1,S2), call(P,X), foreach_(P,T,S2).

cofold(P, MakeMachine, PS) :- call(MakeMachine, unfolder(T,TS)), cofold_(P,T,PS,TS).
cofold_(P,T,PS1,TS1) :- call(T,X,TS1,TS2), call(P,X,PS1,PS2), cofold_(P,T,PS2,TS2).

% bulding unfolding predicates
:- meta_predicate scanner(2,1,-), scan(2,3,-,+,-), scan0(2,?,?,?), :>(1,2,-), >>(2,2,+,-).
scanner(Sel, Setup, machines:scan(Sel, Step)) :- call(Setup,Step).
scan(Sel,Trans,X,P1,P2) :- call(Trans,LP,P1,P2), call(Sel,t(LP,P1,P2),X).
scan0(Trans,S1,S1,S2)   :- call(Trans,S1,S2).

%% :>(+P:pred(-A), +Q:pred(+A,-B), -Y:B) is det.
% machine composition: generator :> transducer --> generator
:>(U,T,M) :- call(U, Unfolder), call(T, Unfolder, M).

%% >>(+P:pred(+A,), +Q:pred(+A,-B), -Y:B) is det.
% machine composition: transducer >> transducer --> trandsducer
>>(T1,T2,U,M) :- call(T1, U, M1), call(T2, M1, M).

% some predicates for building machings
:- meta_predicate unfolder(3,?,-), mapper(2,+,-), moore(3,2,?,+,-), iterator(1,+,-).

%% unfolder(+T:pred(-X,+S,-S), S0:S, -G:unfolder(X,S)) is det.
unfolder(T,S,unfolder(T,S)).

%% mapper(+F:pred(+X,-Y), +G:unfolder(X,S), -T:unfolder(Y,S)) is det.
mapper(F, unfolder(TA,SA), unfolder(machines:map_step(TA,F), SA)).
map_step(T,F,Y) --> call(T,X), {call(F,X,Y)}.

%% moore(+T:pred(+X,+SB,-SB), +O:pred(+SB,-Y), S0:SB, +G:unfolder(X,SA), -G2:unfolder(Y,pair(SA-SB))) is det.
moore(TB,OB,SB, unfolder(TA,SA), unfolder(machines:moore_step(TA,TB,OB), SA-SB)).
moore_step(TA,TB,OB, Out, SA1-SB1, SA2-SB2) :- call(TA,OA,SA1,SA2), call(TB,OA,SB1,SB2), call(OB,SB2,Out).

%% iterator(+I:pred(-pred(-X,+S,-S)), S0:S, -G:unfolder(pair(X,S),S)) is det.
iterator(Setup, S0, unfolder(machines:it_step(Step),S0)) :- call(Setup,Step).
it_step(Step,X-S1,S1,S2) :- call(Step,X,S1,S2).

%% drop(+N:natural +G:unfolder(X,S), -T:unfolder(X,S)) is det.
drop(N, unfolder(T,S0), unfolder(T,S1)) :- length(X,N), foldl(T,X,S0,S1).

subsample(N, unfolder(T,S0), unfolder(machines:skip(N,T),S0)).
skip(N,T,X,S1,S2) :- length([X|Y],N), foldl(T,[X|Y],S1,S2).

% mean machine
mean(In,Out) :- mean(=(0), add, divby, In, Out).

:- meta_predicate mean(1,3,3,+,-).
mean(Zero,Add,DivBy,In,Out) :- call(Zero,Z), moore(mm_step(Add), mm_out(DivBy), 0-Z, In, Out).
mm_step(Add,X,N-Y,M-Z) :- succ(N,M), call(Add,X,Y,Z).
mm_out(DivBy,N-S,M) :- call(DivBy,N,S,M).

