:- module(ccp_test, []).

:- use_module(library(apply_macros)).

:- use_module(library(memo)).
:- use_module(library(data/pair)).
:- use_module(library(callutils)).
:- use_module(library(listutils), [rep/3, take/3]).
:- use_module(library(math)).
:- use_module(library(delimcc)).
:- use_module(library(rbutils)).
:- use_module(library(prob/strand)).
:- use_module(library(ccprism/machines)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/handlers)).
:- use_module(library(ccprism/switches), [marg_log_prob/3]).
:- use_module(library(ccprism/graph)).
:- use_module(library(ccprism/kbest)).
:- use_module(library(ccprism/learn)).
:- use_module(library(ccprism/mcmc)).
:- use_module(library(ccprism/display)).
:- use_module(library(ccprism)).
:- use_module(models).

% ---- general purpose utilities ----

:- meta_predicate samp(0), samp(0,+,-).
samp(G) :- strand(run_sampling(uniform_sampler,G)).
samp(G) --> run_sampling(uniform_sampler,G).
unfold(N0,M,S) :- succ(N0,N), time(samp(call(take(N)*unfold, M, [_|S]))).

histof(Xs,Hist) :- setof(X-N, aggregate(count,member(X,Xs),N), Hist).
distof(Xs,Dist) :-
   histof(Xs,Hist),
   length(Xs,N),
   maplist(fsnd(divby(N)), Hist, Dist).

nathist(Dom,Ns,H) :-
	setof(N-C, aggregate(count, member(N,Ns), C), NCs),
	maplist(lup(NCs), Dom, H).
lup(NCs,N,C) :- member(N-C,NCs) -> true; C=0.

seq_dist(S,CC) :-
	length(S,NumSamples),
   setof(X-N, aggregate(count,member(X,S),N), HH),
   maplist(fsnd(divby(NumSamples-1)),HH,CC).

user:portray(X) :- float(X), !, format('~5g',[X]).

**(G,N) --> {var(N)} -> rep_var(N,G); rep_nonvar(N,G).
rep_nonvar(N,G) --> {N=<0} -> []; {M is N-1}, call_dcg(G), rep_nonvar(M,G).
rep_var(N,G) --> {N=0}; rep_var(M,G), call_dcg(G), {N is M+1}.

% ---- for examining tables ------
:- meta_predicate goal_tables(0,-).
goal_tables(Goal, TableList) :-
   time(goal_expls_tables(Goal,_,Tables)),
   rb_map(Tables, clean_table, Tables1),
   rb_visit(Tables1, TableList).

clean_table(tab(H,Solns,_), tab(H,SolnsList)) :- rb_visit(Solns, SolnsList).

% for getting solutions and explanations incrementally...
:- meta_predicate run_tab_expl(0,-).
run_tab_expl(G, Expl) :- 
   term_variables(G,Ans), 
   run_tab(run_prob(expl,G,Expl,[]), Ans-Expl).

% ---- Testing small fragment of English grammar -----

:- initialization memo_attach('datasets',[]).

:- persistent_memo dataset(+ground,+nonneg,-list(list(atom))).
dataset(_,N,XX) :-
   length(XX,N),
   biased_sampler(SS),
   strand(run_sampling(SS, maplist(phrase(s), XX))).

dataset(ID,XX) :- browse(dataset(ID,_,XX)).

dataset_goal(ID,sentences(XX)) :- dataset(ID,XX).
sentences(XX) :- maplist(phrase(s),XX).


:- meta_predicate speed_test(4,?,+,+,-,-).
speed_test(CountsPred, PSc, Dataset, Reps, Counts, LogProb) :-
   dataset_goal(Dataset, Goal),
   goal_graph(Goal,G),
   member(PSc-InitSpec, [lin-uniform, log-log(uniform)]),
   graph_params(InitSpec,G,P1),
   time(call(CountsPred, PSc, G, P0, Counts0, LP0)),
   time(rep(Reps, eval(t(P0,Counts0,LP0),P1))),
   copy_term(t(P0,Counts0,LP0), t(P1,Counts,LogProb)).

eval(T,P1) :- copy_term(T,t(P1,_,_)).

rep(0,_) :- !.
rep(N,G) :- N1 is N-1, call(G), rep(N1,G).

% ----- Testing MCMC with the two_dice system ----

dice_gibbs_samples(AA,Spec,K,NumSamples,S) :- unfold(NumSamples, dice_gibbs(AA,K,Spec), S).

dice_gibbs(AA,Stride,Spec>F,M) :-
   goal_graph(maplist(two_dice,[4,4,4]), G),
   graph_params(AA*uniform,G,P0),
	call(call(Spec,G,P0,P0) >> drop(500) >> subsample(Stride) >> mapper(snd*nth1(1)*F), M).

counts(Counts) :- setof( Xs, expl_stats(Xs), Counts).
counts_multiplicities(HH) :-
	setof(S-N, aggregate(count, E^(expl(E), nathist([1,2,3],E,S)), N), HH).

expl(E) :- length(E,3), maplist(between(1,3),E).
expl_stats(Xs) :- length(Xs,3), maplist(between(0,3),Xs), sumlist(Xs,3).

dice_exact_probs(AA, Dist) :-
   rep(3,A,Alphas), A is AA/3,
   counts_multiplicities(HH),
   maplist(pair, CC, NN, HH),
   maplist(pair, CC, Ps, Dist),
   maplist(exp*mul(2)*marg_log_prob(Alphas),CC,Ws),
   maplist(mul,NN,Ws,Ws1),
   stoch(Ws1,Ps,_).

test_mcmc(NumSamples, Sub, Spec, S) :-
	counts(CC),
   member(Spec>F, [gibbs_posterior_machine(counts)>(=), mc_machine(mh)>mcs_counts, mc_machine(gibbs2)>mcs_counts, mc_machine(gibbs)>mcs_counts]),
	unfold(NumSamples, dice_gibbs(2,Sub,Spec>F)
                      >> mapper(ind(CC))
                      >> mean(maplist(=(0)), maplist(add), vec_divby)
                      >> drop(2000),
          S).

ind(Xs,X,Is) :- maplist(eq(X),Xs,Is).
eq(X,Y,I) :- X=Y -> I=1; I=0.
vec_divby(K,X,Y) :- maplist(divby(K),X,Y).

call_copies(Goal, Copies) :- maplist(call_copy(Goal), Copies).
call_copy(Goal, Copy) :- copy_term(Goal, Copy), call(Copy).

:- module(ccp_test).
% vim: ft=prolog
