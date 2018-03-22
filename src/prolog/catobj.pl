:- module(catobj, [
    co2cat/2,
    co_cat_ucat/3,
    co_res_arg/3,
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
