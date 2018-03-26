:- module(catobj, [
    co2cat/2,
    co_cat_ucat/3,
    co_res_arg/3,
    cos_tops_deps/3,
    functor_in/2,
    is_modifier_co/1]).

/** <module> CCG category objects

"Category objects" are CCG categories augmented with IDs that distinguish
different occurrences of the same category within one derivation.
  
A category object is represented as a term =|co(ID, Cat, UCat)|= followed by 0
or more arguments =|/CO|= =|\CO|= where CO are also =|co/3|= terms.

ID is a ground term representing the identity of the top category object.

Cat is its category, with fully specified features.

UCat is the argument category of the functor that consumes this category
object. For example, Cat might be =|s:dcl\np|= while UCat is =|s\np|=.

*/

:- use_module(slashes).
:- use_module(util, [
    maplist/6,
    substitute_sub_term/3,
    write_term_vars/2]).

functor_in(CO, CO).
functor_in(CO, X/_) :-
  functor_in(CO, X).
functor_in(CO, X\_) :-
  functor_in(CO, X).
functor_in(CO, X-_) :-
  functor_in(CO, X).

% Atomic category objects:
co2cat(co(_, Cat, _), Cat).
% Forward-type-raised category objects (special case to prevent X\Y from being treated as an underspecified modifier):
co2cat(X/(X\Y), XCat/(XCat\YCat)) :-
  !,
  co2cat(X, XCat),
  co2cat(Y, YCat).
% Backward-type-raised category objects (special case to prevent X/Y from being treated as an underspecified modifier):
co2cat(X\(X/Y), XCat\(XCat/YCat)) :-
  !,
  co2cat(X, XCat),
  co2cat(Y, YCat).
% Forward modifiers (use YUCat to get back at the original underspecified modifier category):
co2cat(X/co(_, YCat, YUCat), YUCat/YUCat) :-
  co2cat(X, XCat),
  XCat == YCat,
  !.
% Backward modifiers (use YUCat to get back at the original underspecified modifier category):
co2cat(X\co(_, YCat, YUCat), YUCat\YUCat) :-
  co2cat(X, XCat),
  XCat == YCat,
  !.
% Other forward functors:
co2cat(X/Y, XCat/YCat) :-
  co2cat(X, XCat),
  co2cat(Y, YCat).
% Other backward functors:
co2cat(X\Y, XCat\YCat) :-
  co2cat(X, XCat),
  co2cat(Y, YCat).

co_cat_ucat(co(_, Cat, UCat), Cat, UCat).
co_cat_ucat(A/B, C/D, E/F) :-
  co_cat_ucat(A, C, E),
  co_cat_ucat(B, D, F).
co_cat_ucat(A\B, C\D, E\F) :-
  co_cat_ucat(A, C, E),
  co_cat_ucat(B, D, F).

is_modifier_co(CO) :-
  co2cat(CO, X/X),
  !.
is_modifier_co(CO) :-
  co2cat(CO, X\X).

co_res_arg(Res/Arg, Res, Arg).
co_res_arg(Res\Arg, Res, Arg).

cos_tops_deps(COs, TopCOs, Deps) :-
  maplist(co_top_target_deps, COs, TopCOs, TopCOs, TargetTopCOs, Depss),
  append(Depss, Deps0),
  %nl,
  %forall(member(TopCO, TopCOs), write_term_vars(TopCO, [nl(true), module(slashes)])),
  %nl,
  %forall(member(TargetTopCO, TargetTopCOs), write_term_vars(TargetTopCO, [nl(true), module(slashes)])),
  %nl,
  %forall(member(Deps, Depss), write_term_vars(Deps, [nl(true), module(slashes)])),
  %nl,
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).

% Resolve dependent:
resolve_targets([target(TopCO)-Head|Deps0], TopCOs, TargetTopCOs, [TargetTopCO-Head|Deps]) :-
  nth1(N, TopCOs, TopCO),
  !,
  nth1(N, TargetTopCOs, TargetTopCO),
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).
% Dependent is not the TopCO of any word - happens with complex arguments. Skip.
resolve_targets([target(_)-_|Deps0], TopCOs, TargetTopCOs, Deps) :-
  !,
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).
% Resolve head:
resolve_targets([Dependent-target(TopCO)|Deps0], TopCOs, TargetTopCOs, [Dependent-TargetTopCO|Deps]) :-
  nth1(N, TopCOs, TopCO),
  !,
  nth1(N, TargetTopCOs, TargetTopCO),
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).
% Head is not the TopCO of any word - happens with complex arguments. Skip.
resolve_targets([_-target(_)|Deps0], TopCOs, TargetTopCOs, Deps) :-
  !,
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).
% Dependency already resolved:
resolve_targets([Dep|Deps0], TopCOs, TargetTopCOs, [Dep|Deps]) :-
  resolve_targets(Deps0, TopCOs, TargetTopCOs, Deps).
% End of recursion:
resolve_targets([], _, _, []).

co_top_target_deps(CO, TopCO, TargetTopCO0, TargetTopCO, [TargetTopCO0-target(ArgTopCO)|Deps]) :-
  is_modifier_co(CO),
  co_res_arg(CO, ResCO, ArgCO),
  !,
  co_top(ArgCO, ArgTopCO), % TODO should we do target calculations here??
  co_top_target_deps(ResCO, TopCO, ArgTopCO, TargetTopCO, Deps).
co_top_target_deps(CO, TopCO, TargetTopCO0, TargetTopCO, [target(ArgTopCO)-TargetTopCO0|Deps]) :-
  co_res_arg(CO, ResCO, ArgCO),
  !,
  co_top(ArgCO, ArgTopCO), % TODO should we do target calculations here??
  co_top_target_deps(ResCO, TopCO, TargetTopCO0, TargetTopCO, Deps).
co_top_target_deps(CO, CO, TargetTopCO, TargetTopCO, []).

co_top(CO, TopCO) :-
  co_res_arg(CO, ResCO, _),
  !,
  co_top(ResCO, TopCO).
co_top(CO, CO).
