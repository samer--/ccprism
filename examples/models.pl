:- module(models, [ s//0, np//0, vp//0, biased_sampler/1, parse/2
                  , repeat_a//1, sm//0, sml//0, smml//0, aux//0
                  , two_dice/1, two_dice/2, three_dice/1, dice/2, die/3]).

/** <module> Example probabilistic models (see code for commentary) */

:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).
:- use_module(library(ccprism/handlers), [make_lookup_sampler/2]).
:- use_module(library(dcg_core), [rep//2]).

%% iota(+N:natural, L1:list(natural), L2:list(natural)) is det.
%  Difference list version of numlist, useful for switch domains.
iota(0,L,L) :- !.
iota(N,L3,L1) :- succ(M,N), iota(M,L3,[N|L1]).

% some switch declarations
coin +-> iota(2).
die  +-> iota(4).    % tetrahedral die
die(_) +-> iota(3). % impossible three sided die

% models of dice throws
:- cctable three_dice/1, two_dice/2, two_dice/1, dice/2.
three_dice(X)   :- length(Xs,3), maplist(:=(die), Xs), sumlist(Xs,X).
two_dice(X1,X2) :- die := X1, die := X2.
two_dice(X)     :- die(1) := D1, die(2) := D2, X is D1+D2.

% :- use_module(library(clpfd)).
dice(0,0).
dice(N,Z) :- succ(M,N), die := X, dice(M,Y), Z is X+Y.
% dice(N,Z) :- 
%    die := X, 
%    nonneg(Y), Z #= X+Y, 
%    nonneg(M), N #= M+1, 
%    dice(M,Y).
% nonneg(X) :- X #>= 0.

% mode to test handling of variables in answers
:- cctable ssucc/2.
ssucc(X, a(X)).
test(Y,Z) :- (X=1;X=2;X=3), ssucc(A,Y), A=X, ssucc(_,Z).


/* -------------------------- Grammars -------------------------

   Grammars are encoded here in a couple of ways. The first here is mostly like a
   normal Prolog DCG, except that disjunctions need to be controlled by a 
   probabilistic choice. The term expansion =|Head :-> Alternatives|= introduces an
   integer valued switch =|Head|= to select one of the disjuncts in =|Alternatives|=,
   each of which can be a normal DCG body.

   Terminals can be generated the usual way using =|[T]|=, but the process is
   simplified here, where calling =|+SW|= samples a value from SW and outputs it
   directly as a terminal. Hence, =|SW|= is used like a 'pre-terminal' symbol.
*/

:- op(1150,xfx,:->).

term_expansion(H :-> Alts, Exp) :-
   (already_defined(H) -> throw(already_defined(H)); true),
   alts_list(Alts, Bodies),
   length(Bodies, N),
   (  N=1 -> dcg_translate_rule(H --> Alts, Exp)
   ;  iota(N, Is, []),
      head_case(H, I, Case),
      head_switch(H, SW),
      maplist(mkrule(H), Is, Bodies, Clauses),
      maplist(dcg_translate_rule, [(H --> {SW := I}, Case) | Clauses], Rules),
      Exp = [ (SW +-> iota(N)) | Rules]
   ).

already_defined(H) :-
   functor(H,F,A), atom_concat(F,'?',F2), A2 is A+3,
   current_predicate(F2/A2).

mkrule(H,I,B,C-->B) :- head_case(H,I,C).
head_case(Head, I, Case) :-
   Head =.. [F|Args], atom_concat(F,'?',FC),
   Case =.. [FC,I|Args].
head_switch(T, T).
% head_switch(T1, T2) :-
%    T1 =.. [F1|Args], atom_concat(':',F1,F2),
%    T2 =.. [F2|Args].

alts_list(A;As, [A|Bs]) :- !, alts_list(As,Bs).
alts_list(A, [A]).

%% +(PT:switch(A), L1:list(A), L2:list(A)) is nondet.
%  Use switch PT as a preterminal, sampling a value from it to use as a terminal.
:- meta_predicate +(3,?,?).
+Lab --> [T], {Lab := T}.

% --- test grammar ----
:- cctable s//0, np//0, vp//0, pp//0, nom//0.

s --> np, vp.

np :-> +d, nom
     ; +pn
     ; np, pp.

vp :-> +iv
     ; +tv, np
     ; +dv, np, np
     ; vp, pp
     ; +mv, s.

nom :-> +n; +adj, nom.
pp --> +p, np.


%% biased_sampler(-S:switch_sampler) is det.
%  Creates a switch parameter sampler for the grammar which avoids placing
%  too much probability on the recursive branches of the nom//0 and np//0
%  predicates. See library(ccprism/handlers) for information about switch samplers.
biased_sampler(fallback_sampler(LU,uniform_sampler)) :-
   make_lookup_sampler([(models:nom)-[0.8,0.2], (models:np)-[0.3,0.6,0.1]],LU).

% preterminal switch declarations
adj +-> [hot,cold,thin,fat,disgusting,lovely].
pn  +-> [alice, bob, cuthbert, delia, edna].
d   +-> [the,a,some,my]. % ,every,no].
mv  +-> [knew,thought,believed,said].
dv  +-> [gave,made,baked].
tv  +-> [saw, ate, hated, baked, liked, walked, ran, loved, caught].
iv  +-> [lived, worked].
n   +-> [dog,telescope,man,cat,mat,cake,box,floor,face,pie,moose,pyjamas,park].
p   +-> [with,on,under,in,without,by].


% --- frost grammars ----
:- cctable sm//0, sml//0, smml//0, aux//0.

sm --> "a", sm, sm; [].
sml :-> sml, sml, "a"; [].
smml :-> smml, aux; [].
aux --> smml, "a".

repeat_a(I) --> rep(I,"a").


/* This is an alternative grammar system that uses 'pre-stored' tables to avoid
   having to work with difference lists in the grammar predicates. Instead, 
   sequences are 'stored' (similar to assertion, but handled more declaratively
   by using the continuation based tabling system), after which terminals are 
   accessed by position index. The lookup predicate c//1 is itself tabled, and
   works by looking up the stored sequence.

   To parse [the,cat] using, eg np2//0, use =|goal_graph(parse(np2,[the,cat]), G)|=
   ==
*/
:- cctable c//1.
c(T, S-I, S-J) :- ccstored(sequence(S,Ts)), nth0(I,Ts,T), succ(I,J).
parse(NT,Words) :- 
   gensym(seq,S),   ccstore(sequence(S,W),W=Words),
   length(Words,N), call_dcg(NT,S-0,S-N).

:- cctable np2//0, nom2//0.
np2 :-> c(the), nom2; c(a), nom2.
nom2 :-> c(cat); c(mat); c(dog); c(frog).

% ----------- thread local memoisation for random world semantics ----------
:- use_module(library(delimcc), [p_shift/2]).
:- use_module(library(ccstate), [run_state_handler//3]).
:- use_module(library(rbutils)).

:- meta_predicate lazy(1,-).
lazy(P,X) :- p_shift(memo,lazy(P,X)).

run_memo(Goal) :- rb_empty(E), run_state_handler(memo,handle_memo,Goal,E,_).

run_memo(Goal) --> run_state_handler(memo,handle_memo,Goal).
handle_memo(lazy(P,X)) --> rb_get(P,X) -> []; run_memo(call(P,X)), rb_add(P,X).

flip +-> [t,f].
flip(X) :- lazy(:=(flip),X).
two_flip(X,Y) :- flip(X), flip(Y).
