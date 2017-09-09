:- module(graph_tikz, [graph_tikz//1, igraph_tikz//1]).
% For typesetting explanation graphs in TikZ 

:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).


graph_tikz(Graph)     --> seqmap_with_sep(";\n", goal_expls_tikz, Graph).
goal_expls_tikz(G-Es) --> factor_tikz(G), to, group(expl_tikz, Es).
expl_tikz(Fs)         --> and_group(factor_tikz,Fs,[]).
factor_tikz(F)        --> factor_opts(F,[]).

igraph_tikz(Graph)         --> seqmap_with_sep(";\n", igoal_expls_tikz, Graph).
igoal_expls_tikz(G-(P-Es)) --> ifactor_tikz(P-G), to, group(iexpl_tikz, Es).
iexpl_tikz(P-Fs)           --> and_group(ifactor_tikz, Fs, [plabel(above,P,[])]).
ifactor_tikz(P - F)        --> {factor_label_opts(F,Opts)}, factor_opts(F,[plabel(right,P,Opts)]).

factor_label_opts(_:_,[]).
factor_label_opts(_:=_,["param"]).

plabel(Pos,P,Opts) --> "label=", label_val(Pos,P,Opts).
label_val(Pos,P,[]) --> !, label(Pos,P).
label_val(Pos,P,Opts) --> brace((opts(Opts), label(Pos,P))).
label(Pos,P) -->  fmt("~w:$~2f$",[Pos,P]).

factor_opts('^top':_,Opts)  --> box_opts(top,Opts).
factor_opts(_:G,Opts)       --> box_opts(G,Opts).
factor_opts((_:SW):=V,Opts) --> box_opts(SW:=V,["sw"|Opts]).

box_opts(X,Opts) --> qt(X), opts(Opts).

and_group(_,[],Opts) --> !, and(Opts).
and_group(G,[F],_) --> !, call(G,F).
and_group(G,Fs,Opts) --> and(Opts), to, group(G,Fs).

and(Opts) --> brace(("[fresh nodes] and/\"\"",opts(["and"|Opts]))).
group(G,Xs) --> brace(seqmap_with_sep(",",G,Xs)).
qt(T) --> qq(wr(T)).
to --> " -> ".
opts([]) --> !.
opts(Opts) --> sqbr(seqmap_with_sep(",",phrase,Opts)).

