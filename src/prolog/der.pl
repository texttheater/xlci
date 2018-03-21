:- module(der, [
    der2node/2,
    pp_der/1]).

:- use_module(cat, [
    strip_var_features/2]).
:- use_module(catobj, [
    co_cat_ucat/3]).
:- use_module(node, [
    node_co/2]).
:- use_module(slashes).
:- use_module(util, [
    must/1,
    print_indented/3,
    substitute_sub_term/4,
    list_occurrences_of_term/3]).

%%	der2node(+Der, -Node)
%	Converts Boxer's derivation terms to our =|node/4|= format. The latter
%	uses category objects instead of plain categories (see the =catobj=
%	module documentation).
der2node(Der, Node) :-
  % Convert from der to node:
  der2node_(Der, Node0),
  % Extract top category of derivation:
  der_cat(Der, Cat),
  % Assign a corresponding category object to the node:
  node_co(Node0, co(_, Cat, Cat)),
  % Find all top category objects in the node:
  list_occurrences_of_term(co(_, _, _), Node0, COs),
  % Find their (variable) IDs:
  maplist(arg(1), COs, IDs),
  % Bind each such variable to a unique integer ID (by slightly abusing
  % numbervars/1 and then replacing the '$VAR'/1 terms by just the integers):
  numbervars(IDs),
  substitute_sub_term('$VAR'(N), N, Node0, Node).

%%	der2node_(+Der, -Node)
%
%	Converts Boxer's derivation terms to our =|node/3|= format, but leaves
%	category object IDs uninstantiated
der2node_(fa(_Cat, Sem, t(TCSem, _/UCat2, 'ø', _), ODer), node(_X, Sem, tc(TCSem), [ONode])) :-
  !,
  node_co(ONode, Y),
  must(der2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(tc(_NewCat, _OldCat, Sem, ODer), node(_X, Sem, tc(nil), [ONode])) :- % nil = HACK
  !,
  node_co(ONode, Y),
  must(der2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, Cat2).
der2node_(fa(_Cat, Sem, Der1, Der2), node(Y, Sem, comp(0, f), [Node1, Node2])) :-
  Der1 = t(lam(A, B), C/D, _, _),
  A == B,
  C == D,
  !,
  node_co(Node1, Y/Y),
  node_co(Node2, Y),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  der_cat(Node1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(fa(_Cat, Sem, Der1, Der2), node(X, Sem, comp(0, f), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  der_cat(Der1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, ODer, t(TCSem, _\UCat2, 'ø', _)), node(_X, Sem, tc(TCSem), [ONode])) :-
  !,
  node_co(ONode, Y),
  must(der2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, Der2, Der1), node(Y, Sem, comp(0, b), [Node2, Node1])) :-
  Der1 = t(lam(A, B), C\D, _, _),
  A == B,
  C == D,
  !,
  node_co(Node2, Y),
  node_co(Node1, Y\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  der_cat(Der1, _\UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, Der2, Der1), node(X, Sem, comp(0, b), [Node2, Node1])) :-
  node_co(Node2, Y),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  der_cat(Der1, _\UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(fc(_Cat, Sem, Der1, Der2), node(X/Z, Sem, comp(1, f), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y/Z),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_cat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2). % FIXME: Y can be a functor??
der2node_(bc(_Cat, Sem, Der2, Der1), node(X\Z, Sem, comp(1, b), [Node2, Node1])) :-
  node_co(Node2, Y\Z),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_cat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(fxc(_Cat, Sem, Der1, Der2), node(X\Z, Sem, comp(1, f), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y\Z),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_cat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(bxc(_Cat, Sem, Der2, Der1), node(X/Z, Sem, comp(1, b), [Node2, Node1])) :-
  node_co(Node2, Y/Z),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_cat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
% TODO generalized harmonic/crossed composition
der2node_(conj(Cat\Cat, CSem, t(TSem, conj:Cat, Form, Atts), Der2), Node) :-
  must(der2node_(fa(Cat\Cat, CSem, t(TSem, (Cat\Cat)/Cat, Form, Atts), Der2), Node)). % HACK
der2node_(ftr(_Cat, _OldCat, Sem, Der), node(X/(X\Y), Sem, ftr, [Node])) :-
  node_co(Node, Y),
  must(der2node_(Der, Node)).
der2node_(btr(_Cat, _OldCat, Sem, Der), node(X\(X/Y), Sem, btr, [Node])) :-
  node_co(Node, Y),
  must(der2node_(Der, Node)).
der2node_(t(Sem, _Cat, Form, Atts), node(_, Sem, t(Form, Atts), [])).

der_cat(t(_, Cat0, _, _), Cat) :-
  !,
  %strip_var_features(Cat0, Cat).
  Cat0 = Cat.
der_cat(Der, Cat) :-
  arg(1, Der, Cat0),
  %strip_var_features(Cat0, Cat).
  Cat0 = Cat.
% HACK: there are two kinds of categories that can have variable features in the output:
% (1) type-raised categories
% (2) modifier categories
% We want to get rid of the variable features for (2) but not for (1).
% parse2der does not add the variables to modifiers, so we're good.
% Boxer does add the variables to modifiers, so if we want to use Boxer output again,
% we'll have to do something more clever.

topcat(0, Cat, Cat) :-
  !.
topcat(N, X/_, TopCat) :-
  M is N - 1,
  topcat(M, X, TopCat).
topcat(N, X\_, TopCat) :-
  M is N - 1,
  topcat(M, X, TopCat).

pp_der(Der) :-
  print_indented(Der, [t(_, _, _, _), lam(_, _), _\_, _/_], [module(slashes)]).
