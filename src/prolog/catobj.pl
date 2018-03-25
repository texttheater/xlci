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
    substitute_sub_term/3]).

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
  append(Depss, Deps).

% FIXME When detecting modification, we use Arg as the new target head and pass
% it down, but not yet up. So CO's that take this CO as an argument still refer
% to the modifier, not to the head. For example, in "Heather è una ragazza
% molto bella", the CO for "una" thinks its argument's head is "molto" while
% it's actually "ragazza".
co_top_target_deps(CO, TopCO, TargetTopCO0, TargetTopCO, [TargetTopCO0-Arg|Deps]) :-
  is_modifier_co(CO),
  co_res_arg(CO, Res, Arg),
  !,
  co_top_target_deps(Res, TopCO, Arg, TargetTopCO, Deps).
co_top_target_deps(CO, TopCO, TargetTopCO0, TargetTopCO, [Arg-TargetTopCO0|Deps]) :-
  co_res_arg(CO, Res, Arg),
  !,
  co_top_target_deps(Res, TopCO, TargetTopCO0, TargetTopCO, Deps).
co_top_target_deps(CO, CO, TargetTopCO, TargetTopCO, []).
