:- module(ccp_display, [print_tree/1]).

:- use_module(library(data/tree),   [print_tree/2]).

print_tree(T) :- tree_to_tree(T,T1), write('  '), print_tree('  ', T1), nl.

tree_to_tree(\P, node(p(P),[])).
tree_to_tree((_:SW):=Val, node(t(SW:=Val),[])).
tree_to_tree((_:Head) - Expls, node(nt(Label), Subnodes)) :-
   functor(Head,Label,_),
   exclude(=(const), Expls, Expls1),
   maplist(tree_to_tree, Expls1, Subnodes).

user:portray(node(nt(Label))) :- print(Label).
user:portray(node(t(Data))) :- write('|'), print(Data).
user:portray(node(p(Prob))) :- write('@'), print(Prob).
