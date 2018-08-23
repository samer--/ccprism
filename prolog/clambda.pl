:- module(clambda, [run_lambda_compiler/1, clambda/2]).
/** <module> Compiled lambdas */
:- use_module(library(delimcc), [p_reset/3, p_shift/2]).

clambda(Lambda, Pred) :- p_shift(clambda, compile(Lambda,Pred)).

:- meta_predicate run_lambda_compiler(0).
run_lambda_compiler(Goal) :- run(Goal, []).
run(Goal,Used) :- p_reset(clambda, Goal, Status), cont(Status, Used).

cont(done,Used) :- maplist(retractall, Used).
cont(susp(compile(lambda(Args, Body), clambda:dpred(I)), Cont), Used) :-
   flag(clambda, I, I+1), Head =.. [dpred, I|Args],
   assert(Head :- Body), run(Cont, [Head | Used]).
