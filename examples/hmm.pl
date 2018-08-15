:- module(hmm, [hmm/2, hmm_db/3, hmm_with_end/2, hmm_with_end_db/3, assert_seq/2]).

:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).

obs(_)  +-> [a, c, g, t].
next(_) +-> [x, y].
next_with_end(_) +-> [x, y, end].

:- cctable hmm/2, hmm_db/3, hmm_with_end/2, hmm_with_end_db/3.

:- dynamic seq_length/2, seq_nth/3, nth_seq/3.

hmm_db(Seq, _, T) :- seq_length(Seq, T).
hmm_db(Seq, S1,T0) :-
   T1 is T0 + 1,
   nth_seq(T1, Seq, X1),
   next(S1) := S2, obs(S1) := X1,
   hmm_db(Seq,S2,T1).

hmm_with_end(end, []).
hmm_with_end(S1,[X1|XX]) :- S1 \= end,
   next_with_end(S1) := S2, obs(S1) := X1,
   hmm_with_end(S2,XX).

hmm_with_end_db(Seq, end, T) :- seq_length(Seq, T).
hmm_with_end_db(Seq, S1,T0) :- S1 \= end,
   T1 is T0 + 1,
   nth_seq(T1, Seq, X1),
   next_with_end(S1) := S2, obs(S1) := X1,
   hmm_with_end_db(Seq,S2,T1).

hmm(_, []).
hmm(S1,[X1|XX]) :-
   next(S1) := S2, obs(S1) := X1,
   hmm(S2,XX).

hmm(XX) :- hmm(x,XX).

assert_seq(Seq, XX) :-
   length(XX, L),
   retractall(seq_length(Seq, _)),
   retractall(seq_nth(Seq, _, _)),
   retractall(nth_seq(_, Seq, _)),
   assert(seq_length(Seq, L)),
   forall(nth1(T, XX, X), assert(seq_nth(Seq, T, X))),
   forall(nth1(T, XX, X), assert(nth_seq(T, Seq, X))).
