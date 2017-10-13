:- module(crp, [ crp_empty/1, crp_sample/4
               , dp//3, dp//2, tdp//2
               , dp_dump/2
               , tfoldl_/3, tfoldl//2
               , run_st/1, run_st_/1
               , lazy/2, mem/2, gem/2, hgem/3
               , abc/1, abc//1
               , new_dp1//3
               , new_dp3/3
               , abc_sw//1, abc_prior/2
               , samp_st/1, samp_ref/1
               , data/3, gen/2 ]).

/** <module> CRPs and stochastic memoisation */

:- use_module(library(apply_macros)).
:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/handlers)).
:- use_module(library(math), [mul/3, add/3, stoch/3]).
:- use_module(library(listutils), [take/3]).
:- use_module(library(data/pair), [pair/3, fst/2]).
:- use_module(library(data/store)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_macros)).
:- use_module(library(ccref)).
:- use_module(library(ccstate)).
:- use_module(library(plrand)).
:- use_module(library(prob/tagless)).

% :- use_module(library(autodiff2)).

% stoch(Ws, Ps, Tot) :-
%    foldl(add, Ws, 0.0, Tot),
%    pow(-1,Tot,InvTot),
%    maplist(mul(InvTot), Ws, Ps).

% dby(X,Y,DYDX) :- deriv(Y,X,DYDX).
% target(Y) :- deriv(Y,Y,1.0).


% tabled fold
:- cctable tfoldl/4.
:- meta_predicate tfoldl(3,?,?,?).
tfoldl(_,[]) --> [].
tfoldl(P,[X|Xs]) --> call(P,X), tfoldl(P,Xs).

% tabled fold, discarding final state
:- cctable tfoldl_/3.
:- meta_predicate tfoldl_(3,?,?).
tfoldl_(_,[],_).
tfoldl_(P,[X|Xs],S) :- call(P,X,S,S1), tfoldl_(P,Xs,S1).


% calling with state
:- meta_predicate run_st(//), run_st_(1).
run_st(G) :- store_new(E), call_dcg(G,E,_).
run_st_(G) :- store_new(E), call(G,E).

% Run sampler with additional state
:- meta_predicate samp_st(//), samp_ref(0).
samp_st(G) :- with_brs(rs, run_sampling(uniform_sampler,run_st(G))).
samp_ref(G) :- with_brs(rs, run_sampling(uniform_sampler,run_ref(G))).

% state is store, call P with pair(CRP,store) state
:- meta_predicate ref_appl(+,2,+,-).
ref_appl(Ref,P) -->
   store_get(Ref,X1), run_left(P,X1,X2),
   store_set(Ref,X2).

% creating some data
:- dynamic seq/2.
data(I,L,X) :- seq(I,Y), take(L,Y,X).
gen(A,I) :-
   length(Xs,100),
   samp_st((new_dp1(A,abc,G),tfoldl(G, Xs))),
   assert(seq(I,Xs)).


% DP models
abc(X) :- dist([0.5,0.3,0.2],[a,b,c],X).
abc(X) --> {abc(X)}.
abc_sw +-> [a,b,c].

abc_prior(A, [SW-As]) :- abc_sw(SW,_,_), maplist(mul(A),[0.5,0.3,0.2],As).

:- cctable tdp//2.
tdp(A,X) --> dp(A,X).
dp(A,X) --> dp(A,abc,X).

:- meta_predicate new_dp1(3,+,-,+,-), new_dp3(1,+,-).
new_dp1(H,A,crp:dp1(Ref,A,H)) --> store_add([],Ref).
new_dp3(H,A,crp:dp3(Ref,A,H)) :- ref_new([],Ref).
dp1(Ref,A,H,X) --> ref_appl(Ref, dpl(A,H,X)).
dp3(Ref,A,H,X) :- ref_app_ref(Ref, dp(A,H,X)).

dp_dump(crp:dp3(Ref,_,_),C) :- ref_get(Ref,C).

:- meta_predicate lazy(1,-), mem(2,-).
lazy(P,crp:lazy_(R,P)) :- ref_new(nothing, R).
lazy_(R,P,X) :- ref_maybe_app_ref(R, lazy_(P,X)).
lazy_(P,X,nothing,just(just(X))) :- !, call(P,X).
lazy_(_,X,just(X),nothing).

mem(P, crp:mem_(R,P)) :- rb_empty(E), ref_new(E, R).
mem_(R,P,X,Y) :- ref_maybe_app_ref(R, mem_(P,X,Y)).
mem_(_,X,Y,M,nothing) :- rb_lookup(X,Y,M), !.
mem_(P,X,Y,M1,just(M2)) :- call(P,X,Y), rb_insert_new(M1, X, Y, M2).

gem_beta(dp(A),_,1,A).
gem_beta(py(A,D),K,B1,B0) :- B1 is 1-D, B0 is A+K*D.

stick(Param,K,X) :- gem_beta(Param,K,A,B), sample(beta(A,B),X).
stick(Param,H,K,X-Y) :- stick(Param,K,X), call(H,Y).

gem(Param, crp:gem_(Sticks)) :- mem(stick(Param),Sticks).
gem_(Sticks, I) :- gem_(Sticks, 1, I).
gem_(Sticks, K, I) :-
   call(Sticks, K, P),
   bernoulli(P, X),
   (X=1 -> I=K; J is K+1, gem_(Sticks, J, I)).

hgem(Param, H, crp:hgem_(Sticks)) :- mem(stick(Param,H),Sticks).
hgem_(Sticks, Y) :- hgem_(Sticks, 1, Y).
hgem_(Sticks, K, Z) :-
   call(Sticks, K, P-Y),
   bernoulli(P, X),
   (X=1 -> Y=Z; J is K+1, hgem_(Sticks, J, Z)).

bernoulli(P,X) :- P0 is 1-P, dist([P0-0, P-1], X).

% paired state, compatible with ref_appl
:- meta_predicate dp(+,3,-,+,-).
dpl(A,H,X) -->
   \< crp_sample(dp(A),Y),
   ({Y=old(X)}; {Y=new(X)}, \> call(H,X)),
   \< msort.

% state is 1 single CRP state
:- meta_predicate dp(+,1,-,+,-).
dp(A,H,X) -->
   crp_sample(dp(A),Y),
   { Y=old(X); Y=new(X), call(H,X) },
   msort.

crp_sample(Theta, Result, S1, S2) :-
	crp_dist(Theta, S1, Weights, K),
   (  K=0 -> I=0
   ;  stoch(Weights, Probs, _),
      numlist(0, K, Is),
      dist(Probs, Is, I)
   ),
   crp_cont(I, Result, S1, S2).

crp_cont(0, new(X), S1, S2) :- !, add_class(X, S1, S2).
crp_cont(I, old(X), S1, S2) :- inc_class(I, X, S1, S2).

% List of pairs representation
crp_empty([]).

crp_dist(dp(A),   Hist, [A|Counts], K) :- !, maplist(fst,Hist,Counts), length(Counts,K).
crp_dist(py(_,_), [],   [1], 0) :- !.
crp_dist(py(A,D), Hist, [WNew|Ws], K) :- !,
   maplist(fst,Hist,Counts),
   length(Counts,K),
   neg(D,NegD), maplist(add(NegD),Counts,Ws),
   WNew is A + D*K.

add_class(V, Hist, [1-V|Hist]).
inc_class(1, V, [C1-V|Hist], [C2-V|Hist]) :- !, C2 is C1+1.
inc_class(I, V, [H|Hist1], [H|Hist2]) :- J is I-1, inc_class(J, V, Hist1, Hist2).

/* Pair of lists representation
crp_empty(crp([],[])).

crp_dist(dp(A),   crp(Counts,_), [A|Counts], K) :- !, length(Counts,K).
crp_dist(py(_,_), crp([],_),     [1], 0) :- !.
crp_dist(py(A,D), crp(Counts,_), [WNew|Ws], K) :- !,
   length(Counts,K),
   neg(D,NegD),
   maplist(add(NegD),Counts,Ws),
   WNew is A + D*K.

add_class(V, crp(Cs,Vs), crp([1|Cs],[V|Vs])).
inc_class(I, V, crp(C1,Vs), crp(C2,Vs)) :- nth1(I,Vs,V), inc_nth(I,C1,C2).

inc_nth(1,[X|T],[Y|T]) :- !, Y is X+1.
inc_nth(N,[X|T1],[X|T2]) :- M is N-1, inc_nth(M,T1,T2).
*/

neg(X,Y) :- mul(-1.0,X,Y).

user:portray(store(_,S)) :-
   write('<'),
   forall(rb_in(K,V,S), format('~w→~p;',[K,V])),
   write('>').
user:portray(S) :- is_rbtree(S), !,
   write('<'),
   forall(rb_in(K,V,S), format('~w→~p;',[K,V])),
   write('>').
