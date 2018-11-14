:- module(der, [
    der2node/2,
    sder2node/2,
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
    substitute_sub_term/3,
    substitute_sub_term/4,
    list_occurrences_of_term/3]).

%%	der2node(+Der, -Node)
%
%	Converts Boxer's derivation terms to our =|node/4|= format. The latter
%	uses category objects instead of plain categories (see the =catobj=
%	module documentation).
der2node(Der, Node) :-
  % Convert from der to node:
  der2node_(Der, Node0),
  % Extract top category of derivation:
  der_cat(Der, Cat),
  % Assign a corresponding category object to the node:
  node_co(Node0, CO),
  once(co_cat_ucat(CO, Cat, Cat)),
  %node_co(Node0, co(_, Cat, Cat)),
  % Find all top category objects in the node:
  list_occurrences_of_term(co(_, _, _), Node0, COs),
  % Find their (variable) IDs:
  maplist(arg(1), COs, IDs),
  % Bind each such variable to a unique integer ID (by slightly abusing
  % numbervars/1 and then replacing the '$VAR'/1 terms by just the integers):
  numbervars(IDs),
  substitute_sub_term('$VAR'(N), N, Node0, Node1),
  % HACK: bind UCats that are still variable:
  substitute_sub_term(bind_ucat, Node1, Node).

%%	sder2node(+Der, -Node)
%
%       Like der2node/2, but assumes only forward slashes in input, as in
%       xlsp output.
sder2node(Der, Node) :-
  % Convert from der to node:
  sder2node_(Der, Node0),
  % Extract top category of derivation:
  der_cat(Der, Cat),
  % Assign a corresponding category object to the node:
  node_co(Node0, CO),
  once(co_cat_ucat(CO, Cat, Cat)),
  %node_co(Node0, co(_, Cat, Cat)),
  % Find all top category objects in the node:
  list_occurrences_of_term(co(_, _, _), Node0, COs),
  % Find their (variable) IDs:
  maplist(arg(1), COs, IDs),
  % Bind each such variable to a unique integer ID (by slightly abusing
  % numbervars/1 and then replacing the '$VAR'/1 terms by just the integers):
  numbervars(IDs),
  substitute_sub_term('$VAR'(N), N, Node0, Node1),
  % HACK: bind UCats that are still variable:
  substitute_sub_term(bind_ucat, Node1, Node).

bind_ucat(co(ID, Cat, UCat), co(ID, Cat, Cat)) :-
  nonvar(Cat),
  var(UCat).

%%	der2node_(+Der, -Node)
%
%	Converts Boxer's derivation terms to our =|node/3|= format, but leaves
%	category object IDs uninstantiated
%
%	EasyCCG's limitations apply: generalized composition is supported up to
%	degree 2, and with generalized crossing composition, all delayed
%	arguments must be on the same side.
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
der2node_(fa(_Cat, Sem, Der1, Der2), node(Y, Sem, comp(0, f, h), [Node1, Node2])) :-
  Der1 = t(lam(A, B), C/D, _, _),
  A == B,
  C == D,
  !,
  node_co(Node1, Y/Y),
  node_co(Node2, Y),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(fa(_Cat, Sem, Der1, Der2), node(X, Sem, comp(0, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, ODer, t(TCSem, _\UCat2, 'ø', _)), node(_X, Sem, tc(TCSem), [ONode])) :-
  !,
  node_co(ONode, Y),
  must(der2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, Der2, Der1), node(Y, Sem, comp(0, b, h), [Node2, Node1])) :-
  Der1 = t(lam(A, B), C\D, _, _),
  A == B,
  C == D,
  !,
  node_co(Node2, Y),
  node_co(Node1, Y\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(ba(_Cat, Sem, Der2, Der1), node(X, Sem, comp(0, b, h), [Node2, Node1])) :-
  node_co(Node2, Y),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
der2node_(fc(_/_, Sem, Der1, Der2), node(X/Z, Sem, comp(1, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y/Z),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(bc(_\_, Sem, Der2, Der1), node(X\Z, Sem, comp(1, b, h), [Node2, Node1])) :-
  node_co(Node2, Y\Z),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gfc((_/_)/_, Sem, Der1, Der2), node((X/Z2)/Z1, Sem, comp(2, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)/Z1),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gfc((_/_)\_, Sem, Der1, Der2), node((X/Z2)\Z1, Sem, comp(2, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)\Z1),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gbc((_\_)\_, Sem, Der2, Der1), node((X\Z2)\Z1, Sem, comp(2, b, h), [Node2, Node1])) :-
  node_co(Node2, (Y\Z2)\Z1),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gbc((_\_)/_, Sem, Der2, Der1), node((X\Z2)/Z1, Sem, comp(2, b, h), [Node2, Node1])) :-
  node_co(Node2, (Y\Z2)/Z1),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(fxc(_\_, Sem, Der1, Der2), node(X\Z, Sem, comp(1, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y\Z),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(bxc(_/_, Sem, Der2, Der1), node(X/Z, Sem, comp(1, b, x), [Node2, Node1])) :-
  node_co(Node2, Y/Z),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gfxc((_\_)\_, Sem, Der1, Der2), node((X\Z2)\Z1, Sem, comp(2, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y\Z2)\Z1),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gfxc((_\_)/_, Sem, Der1, Der2), node((X\Z2)/Z1, Sem, comp(2, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y\Z2)/Z1),
  must(der2node_(Der1, Node1)),
  must(der2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gbxc((_/_)/_, Sem, Der2, Der1), node((X/Z2)/Z1, Sem, comp(2, b, x), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)/Z1),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
der2node_(gbxc((_/_)\_, Sem, Der2, Der1), node((X/Z2)\Z1, Sem, comp(2, b, x), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)\Z1),
  node_co(Node1, X\Y),
  must(der2node_(Der2, Node2)),
  must(der2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _\UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
%der2node_(conj(Cat\Cat, CSem, t(TSem, conj:Cat, Form, Atts), Der2), Node) :-
%  must(der2node_(fa(Cat\Cat, CSem, t(TSem, (Cat\Cat)/Cat, Form, Atts), Der2), Node)). % HACK
der2node_(ftr(_Cat, _OldCat, Sem, Der), node(X/(X\Y), Sem, ftr, [Node])) :-
  node_co(Node, Y),
  must(der2node_(Der, Node)).
der2node_(btr(_Cat, _OldCat, Sem, Der), node(X\(X/Y), Sem, btr, [Node])) :-
  node_co(Node, Y),
  must(der2node_(Der, Node)).
der2node_(t(Sem, _Cat, Form, Atts), node(_, Sem, t(Form, Atts), [])).

%%      sder2node_(+Der, -Node)
%
%       Like der2node_/2, but uses only forward slashes, as in
%       xlsp output.
sder2node_(fa(_Cat, Sem, t(TCSem, _/UCat2, 'ø', _), ODer), node(_X, Sem, tc(TCSem), [ONode])) :-
  !,
  node_co(ONode, Y),
  must(sder2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, UCat2).
sder2node_(tc(_NewCat, _OldCat, Sem, ODer), node(_X, Sem, tc(nil), [ONode])) :- % nil = HACK
  !,
  node_co(ONode, Y),
  must(sder2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, Cat2).
%sder2node_(fa(_Cat, Sem, Der1, Der2), node(Y, Sem, comp(0, f, h), [Node1, Node2])) :-
%  Der1 = t(lam(A, B), C/D, _, _),
%  A == B,
%  C == D,
%  !,
%  node_co(Node1, Y/Y),
%  node_co(Node2, Y),
%  must(sder2node_(Der1, Node1)),
%  must(sder2node_(Der2, Node2)),
%  der_cat(Der2, Cat2),
%  der_ucat(Der1, _/UCat2),
%  co_cat_ucat(Y, Cat2, UCat2).
sder2node_(fa(_Cat, Sem, Der1, Der2), node(X, Sem, comp(0, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
sder2node_(ba(_Cat, Sem, ODer, t(TCSem, _/UCat2, 'ø', _)), node(_X, Sem, tc(TCSem), [ONode])) :-
  !,
  node_co(ONode, Y),
  must(sder2node_(ODer, ONode)),
  der_cat(ODer, Cat2),
  co_cat_ucat(Y, Cat2, UCat2).
%sder2node_(ba(_Cat, Sem, Der2, Der1), node(Y, Sem, comp(0, b, h), [Node2, Node1])) :-
%  Der1 = t(lam(A, B), C/D, _, _),
%  A == B,
%  C == D,
%  !,
%  node_co(Node2, Y),
%  node_co(Node1, Y/Y),
%  must(sder2node_(Der2, Node2)),
%  must(sder2node_(Der1, Node1)),
%  der_cat(Der2, Cat2),
%  der_ucat(Der1, _/UCat2),
%  co_cat_ucat(Y, Cat2, UCat2).
sder2node_(ba(_Cat, Sem, Der2, Der1), node(X, Sem, comp(0, b, h), [Node2, Node1])) :-
  node_co(Node2, Y),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, Cat2, UCat2).
sder2node_(fc(_/_, Sem, Der1, Der2), node(X/Z, Sem, comp(1, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y/Z),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(bc(_/_, Sem, Der2, Der1), node(X/Z, Sem, comp(1, b, h), [Node2, Node1])) :-
  node_co(Node2, Y/Z),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gfc((_/_)/_, Sem, Der1, Der2), node((X/Z2)/Z1, Sem, comp(2, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)/Z1),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gfc((_/_)/_, Sem, Der1, Der2), node((X/Z2)/Z1, Sem, comp(2, f, h), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)/Z1),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gbc((_/_)/_, Sem, Der2, Der1), node((X/Z2)/Z1, Sem, comp(2, b, h), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)/Z1),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gbc((_/_)/_, Sem, Der2, Der1), node((X/Z2)/Z1, Sem, comp(2, b, h), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)/Z1),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(fxc(_/_, Sem, Der1, Der2), node(X/Z, Sem, comp(1, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, Y/Z),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(bxc(_/_, Sem, Der2, Der1), node(X/Z, Sem, comp(1, b, x), [Node2, Node1])) :-
  node_co(Node2, Y/Z),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(1, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gfxc((_/_)/_, Sem, Der1, Der2), node((X/Z2)/Z1, Sem, comp(2, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)/Z1),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gfxc((_/_)/_, Sem, Der1, Der2), node((X/Z2)/Z1, Sem, comp(2, f, x), [Node1, Node2])) :-
  node_co(Node1, X/Y),
  node_co(Node2, (Y/Z2)/Z1),
  must(sder2node_(Der1, Node1)),
  must(sder2node_(Der2, Node2)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gbxc((_/_)/_, Sem, Der2, Der1), node((X/Z2)/Z1, Sem, comp(2, b, x), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)/Z1),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(gbxc((_/_)/_, Sem, Der2, Der1), node((X/Z2)/Z1, Sem, comp(2, b, x), [Node2, Node1])) :-
  node_co(Node2, (Y/Z2)/Z1),
  node_co(Node1, X/Y),
  must(sder2node_(Der2, Node2)),
  must(sder2node_(Der1, Node1)),
  der_cat(Der2, Cat2),
  topcat(2, Cat2, TopCat),
  der_ucat(Der1, _/UCat2),
  co_cat_ucat(Y, TopCat, UCat2).
sder2node_(conj(Cat/Cat, CSem, t(TSem, conj:_, Form, Atts), Der2), Node) :-
  must(sder2node_(fa(Cat/Cat, CSem, t(TSem, (Cat/Cat)/Cat, Form, Atts), Der2), Node)). % HACK
sder2node_(ftr(_Cat, _OldCat, Sem, Der), node(X/(X/Y), Sem, ftr, [Node])) :-
  node_co(Node, Y),
  must(sder2node_(Der, Node)).
sder2node_(btr(_Cat, _OldCat, Sem, Der), node(X/(X/Y), Sem, btr, [Node])) :-
  node_co(Node, Y),
  must(sder2node_(Der, Node)).
sder2node_(t(Sem, _Cat, Form, Atts), node(_, Sem, t(Form, Atts), [])).

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

% HACK: for determining UCats, we behave mostly like der_cat/2, but just return
% a variable in case of type raising to avoid accidentally binding UCats.
der_ucat(t(_, Cat, _, _), Cat) :-
  !.
der_ucat(ftr(_, _, _, _), _) :-
  !.
der_ucat(btr(_, _, _, _), _) :-
  !.
der_ucat(fxc(_, _, ftr(_, _, _, _), _), _) :-
  !.
der_ucat(fxc(_, _, _, btr(_, _, _, _)), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _)))), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _))))), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _)))))), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _))))))), _) :-
  !.
der_ucat(gfxc(_, _, _, fxc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(fxc(_, _, _, gfxc(_, _, _, gfxc(_, _, _, btr(_, _, _, _)))), _) :-
  !.
der_ucat(fxc(_, _, _, gfxc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(fxc(_, _, _, fxc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(fxc(_, _, _, bc(_, _, btr(_, _, _, _), _)), _) :-
  !.
der_ucat(fxc(_, _, _, bc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(gfxc(_, _, _, btr(_, _, _, _)), _) :-
  !.
der_ucat(gfxc(_, _, _, gfxc(_, _, _, btr(_, _, _, _))), _) :-
  !.
der_ucat(gfxc(_, _, _, gfxc(_, _, _, gfxc(_, _, _, btr(_, _, _, _)))), _) :-
  !.
der_ucat(bxc(_, _, ftr(_, _, _, _), _), _) :-
  !.
der_ucat(bxc(_, _, bxc(_, _, ftr(_, _, _, _), _), _), _) :-
  !.
der_ucat(bxc(_, _, bxc(_, _, bxc(_, _, ftr(_, _, _, _), _), _), _), _) :-
  !.
der_ucat(bxc(_, _, bxc(_, _, bxc(_, _, bxc(_, _, ftr(_, _, _, _), _), _), _), _), _) :-
  !.
der_ucat(bxc(_, _, bxc(_, _, bxc(_, _, bxc(_, _, bxc(_, _, ftr(_, _, _, _), _), _), _), _), _), _) :-
  !.
der_ucat(bxc(_, _, _, btr(_, _, _, _)), _) :-
  !.
% TODO Does this work?
% TODO Why does this work?
% TODO Are there further cases we need to consider?
der_ucat(Der, Cat) :-
  arg(1, Der, Cat).

topcat(0, Cat, Cat) :-
  !.
topcat(N, X/_, TopCat) :-
  M is N - 1,
  topcat(M, X, TopCat).
topcat(N, X\_, TopCat) :-
  M is N - 1,
  topcat(M, X, TopCat).

pp_der(Der) :-
  print_indented(Der, [t(_, _, _, _), lam(_, _), app(_, _), _\_, _/_], [module(slashes), fullstop(true)]).
