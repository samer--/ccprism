:- module(callops, [op(600,xfy,>:), op(600,yfx,:>), op(600,xfy,>>), op(600,xfy,::), op(700,xfy,:-:), op(200,fx,^), op(900,yfx,$$),
                    (>:)/3, (:>)/3, (>>)/4, (::)/3, (:-:)/3, (^)//2, (**)//2, ($$)/2, ($$)/3]).

:- meta_predicate >>(2,2,?,?), >:(2,1,?), :>(1,2,?), :-:(1,1,?), ^(1,?,?,?), **(//,?,?,?), ::(1,1,?), $$(1,?), $$(2,?,?).

>:(P,Q,X) :- call(P,X,Y), call(Q,Y).
:>(P,Q,Y) :- call(P,X), call(Q,X,Y).
>>(P,Q,X,Z) :- call(P,X,Y), call(Q,Y,Z).
::(F,G,X) :- call(F,X), call(G,X).
$$(P,X)   :- call(P,X).
$$(P,X,Y) :- call(P,X,Y).

:-:(P1,P2,X1-X2) --> call(P1,X1), call(P2,X2).
:-:(P1,P2,X1-X2) :- call(P1,X1), call(P2,X2).

^(P,X) --> {call(P,X)}.

**(G,N) --> {var(N)} -> rep_var(N,G); rep_nonvar(N,G).
rep_nonvar(N,G) --> {N=<0} -> []; {M is N-1}, call_dcg(G), rep_nonvar(M,G).
rep_var(N,G) --> {N=0}; rep_var(M,G), call_dcg(G), {N is M+1}.

