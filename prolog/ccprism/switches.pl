:- module(ccp_switches, [ map_sw/3, map_swc/3, map_sum_sw/3, map_sum_sw/4, map_swc/4
                        , sw_samples/2, sw_expectations/2, sw_log_prob/3, sw_posteriors/3, marg_log_prob/3
                        , sw_init/3, dirichlet/2
                        ]).

/** <module> Tools for working with lists of switch parameters

   Switch parameters are represented as a list of pairs:
   ==
   sw_params == list(pair(switch(_), list(number))).
   ==
   Each switch term is associated with a list of numbers, one for each value
   the switch can take. The meaning of the numbers is context dependent, but is
   usually either a normalised probability distribution over the values or the
   parameters for a Dirichlet distribution over switch value distributions.
*/

:- use_module(library(callutils),   [(*)/4, const/3]).
:- use_module(library(data/pair),   [fsnd/3, snd/2]).
:- use_module(library(plrand),      [log_prob_dirichlet/3, log_partition_dirichlet/2]).
:- use_module(library(prob/tagless),[dirichlet//2]).
:- use_module(effects,  [sample/2]).
:- use_module(lazymath, [add/3, mul/3, stoch/2, map_sum/3, map_sum/4]).

dirichlet(As,Ps) :- sample(dirichlet(As),Ps).

% manipulating parameter lists
:- meta_predicate map_sw(2,?,?), map_swc(2,?,?), map_swc(3,?,?,?), map_sum_sw(2,+,-), map_sum_sw(3,+,+,-).

map_sw(P,X,Y) :- maplist(fsnd(P),X,Y).
map_swc(P,X,Y) :- map_sw(fsnd(maplist(P)),X,Y).
map_swc(P,X,Y,Z) :- maplist(fsnd3(fsnd3(maplist(P))),X,Y,Z).
map_sum_sw(P,X,Sum) :- map_sum(P*snd,X,Sum).
map_sum_sw(P,X,Y,Sum) :- map_sum(f2sw1(P),X,Y,Sum).
fsnd3(P,A-X,A-Y,A-Z) :- call(P,X,Y,Z).
f2sw1(P,SW-X,SW-Y,Z) :- call(P,X,Y,Z).

sw_posteriors(Prior,Eta,Post) :- map_swc(add,Eta,Prior,Post).
sw_expectations(Alphas,Probs) :- map_sw(stoch,Alphas,Probs).
sw_samples(Alphas,Probs)      :- map_sw(dirichlet,Alphas,Probs).
sw_log_prob(Alphas,Probs,LP)  :- map_sum_sw(log_prob_dirichlet,Alphas,Probs,LP).
sw_marg_log_prob(Prior,Eta,LP):- map_sum_sw(marg_log_prob,Prior,Eta,LP).

marg_log_prob(Prior,Eta,LP) :-
   maplist(add,Prior,Eta,Post),
   maplist(log_partition_dirichlet,[Prior,Post],[Bot,Top]),
   LP is Top - Bot.

%% sw_init(+Spec:sw_init_spec, +SW:switch(A), -SWP:pair(switch(A),list(number))) is det.
%  Initialise parameters for given switch using specification term Spec, which can be:
%     * uniform
%     Uniform probability distribution over switch's values.
%     * unit
%     The value 1 for each value the switch can take.
%     * random
%     Sample probability distribution over switch values from dirichlet([1,1,...]).
%     Requires probabilistic effect sample/2 to be available.
%     * K*Spec
%     Take the result of Spec and multiply all values by K.
%     * log(Spec)
%     Take the result of Spec and take the logarithm of each value.
%     * Spec1+Spec2
%     Add the results of initialising with Spec1 and Spec2
sw_init(Spec, SW, SW-P) :- call(SW,_,Vals,[]), init(Spec, Vals, P).

init(uniform,Vs, Params) :- length(Vs,N), P is 1.0/N, maplist(const(P),Vs,Params).
init(unit,   Vs, Params) :- maplist(const(1.0),Vs,Params).
init(random, Vs, Params) :- call(dirichlet*init(unit), Vs, Params).
init(K*Spec, Vs, Params) :- call(maplist(mul(K))*init(Spec), Vs, Params).
init(S1+S2,  Vs, Params) :- init(S1,Vs,P1), init(S2,Vs,P2), maplist(add,P1,P2,Params).
init(log(Spec), Vs, Params) :- call(maplist(log)*init(Spec), Vs, Params).
