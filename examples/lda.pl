:- module(lda, [lda/5, lda2/5, unif/2, unif//2, mkunif/2, dirichlet/3]).
/* Samplers for Latent Dirichlet Allocation (no inteference) */

:- use_module(library(ccprism/macros)).
:- use_module(library(ccprism/effects)).
:- use_module(library(math), [mul/3, add/3, stoch/3]).
:- use_module(library(listutils), [zip/3, enumerate/2]).
:- use_module(library(data/pair), [pair/3, fst/2]).
:- use_module(library(callutils)).
:- use_module(library(prob/tagless)).
:- use_module(library(lex/biglex)).

% load time execution of body to get head(s)
term_expansion(Head := G, Heads) :- findall(Head, call(G), Heads).

places(Xs) := findall(X,place(_,X),Xs).
nouns(Xs) := findall(X,noun(_,_,X,_),Xs).
names(Xs) := findall(X,pname(_,X),Xs).


unif(G,X,S,S) :- call(G,Xs), uniform(Xs,X).
unif(G,X) :- call(G,Xs), uniform(Xs,X).

mkunif(G,D) :- call(G,Xs), length(Xs,N), P is 1/N, maplist(pair(P),Xs,D).

dirichlet(Alpha,Base,Dist) :-
   zip(Probs,Vals,Base),
   zip(Probs1,Vals,Dist),
   maplist(mul(Alpha), Probs, Alphas),
   sample(dirichlet(Alphas),Probs1).

lda(Eta, K, Alpha, L, Docs) :-
   mkunif(nouns,Noun),
   length(Topics, K),
   maplist(dirichlet(Eta,Noun), Topics),
   mkunif(=(Topics), Topic),
   maplist(doc(Alpha,Topic,L), Docs).

doc(Alpha, Topic, L, Doc) :-
   length(Doc, L),
   dirichlet(Alpha,Topic, TopicDist),
   maplist(topic_dist_word(TopicDist), Doc).

topic_dist_word(TopicDist, Word) :-
   dist(TopicDist, Topic),
   dist(Topic, Word).

% this version avoids a lot of (un)zipping and is faster
mkunif2(G,Probs,Vals) :- call(G,Vals), length(Vals,N), P is 1/N, maplist(const(P),Vals,Probs).

dirichlet2(Alpha,Probs,Probs1) :-
   maplist(mul(Alpha), Probs, Alphas),
   sample(dirichlet(Alphas),Probs1).

lda2(Eta, K, Alpha, L, Docs) :-
   mkunif2(nouns,NounProbs,Nouns),
   length(Topics, K),
   maplist(dirichlet2(Eta,NounProbs), Topics),
   mkunif2(=(Topics), TopicProbs, _),
   maplist(doc2(Nouns, Alpha,Topics, TopicProbs,L), Docs).

doc2(Nouns, Alpha, Topics, TopicProbs, L, Doc) :-
   length(Doc, L),
   dirichlet2(Alpha, TopicProbs, TopicDist),
   maplist(topic_dist_word2(Nouns, Topics, TopicDist), Doc).

topic_dist_word2(Nouns, Topics, TopicDist, Word) :-
   dist(TopicDist, Topics, Topic),
   dist(Topic, Nouns, Word).
