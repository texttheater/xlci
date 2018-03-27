:- module(sr, [
    parse/2]).

:- use_module(catobj, [
    functor_in/2]).
:- use_module(slashes).

%:- debug(agenda).

%% parse(+AgendaIn, -AgendaOut)
%
parse(Agenda, _) :-
  length(Agenda, Length),
  (  Length > 256
  -> throw(agenda_limit_exceeded)
  ;  true
  ),
  debug(agenda, '~w items on agenda', [Length, Agenda]),
  fail.
parse(Agenda, Agenda) :-
  maplist(is_finished, Agenda),
  !.
parse(Agenda0, Agenda) :-
  findall(Item,
      ( member(Item0, Agenda0),
        action(Item0, Item)
      ), Agenda1),
  parse(Agenda1, Agenda).

is_finished(item(_, _, _, true)).

action(Item0, Item) :-
  shift(Item0, Item).
action(Item0, Item) :-
  binary(Item0, Item).
action(Item0, Item) :-
  typeraise(Item0, Item).
action(Item0, Item) :-
  typechange(Item0, Item).
action(Item0, Item) :-
  finish(Item0, Item).
action(Item0, Item) :-
  idle(Item0, Item).

shift(item(Stack, [Choices|Queue], TC, false), item([Node|Stack], Queue, TC, false)) :-
  member(Node, Choices).

binary(item([node(RBP, RSem, RRule, RChildren), node(LBP, LSem, LRule, LChildren)|Stack], Queue, TC, false),
    item([node(BP, Sem, Rule, [node(LBP, LSem, LRule, LChildren), node(RBP, RSem, RRule, RChildren)])|Stack], Queue, TC, false)) :-
    binary_rule(Rule, LBP, RBP, BP, LSem, RSem, Sem),
    \+ \+ normal_form(LRule, RRule, Rule). % TODO also support punctuation/coordination rules?

% Type raising can occur when there is a suitable functor in the queue (ftr) or
% on the stack (btr).
typeraise(item([node(X, OSem, ORule, OChildren)|Stack], Queue, TC, false),
    item([node(Y/(Y\X), lam(F, app(F, OSem)), ftr, [node(X, OSem, ORule, OChildren)])|Stack], Queue, TC, false)) :-
  member(Position, Queue),
  member(node(CO, _, _, _), Position),
  functor_in(Y\X, CO),
  X \= Y. % prevent type-raising towards semantically empty category objects
typeraise(item([node(X, OSem, ORule, OChildren)|Stack], Queue, TC, false),
    item([node(Y\(Y/X), lam(F, app(F, OSem)), btr, [node(X, OSem, ORule, OChildren)])|Stack], Queue, TC, false)) :-
  member(node(CO, _, _, _), Stack),
  functor_in(Y/X, CO),
  X \= Y. % prevent type-raising towards semantically empty category objects

typechange(item([node(Y, OSem, ORule, OChildren)|Stack], Queue, TC0, false), 
    item([node(X, app(TCSem, OSem), tc(TCSem), [node(Y, OSem, ORule, OChildren)])|Stack], Queue, TC, false)) :-
  select(tc(X-Y, TCSem), TC0, TC).

finish(item([Parse], [], TC, false), item([Parse], [], TC, true)).

idle(item(Stack, Queue, TC, true), item(Stack, Queue, TC, true)).

% Rule schemas are represented as comp(Degree, Direction, Crossed) where
% Direction is f for forward or b for backward. Crossed is x if it's a
% crossed compostion rule, h otherwise. Following EasyCCG, we limit the
% composition degree to 2.
binary_rule(comp(0, f, h), X/Y, Y, X, Sem1, Sem2, app(Sem1, Sem2)).
binary_rule(comp(0, b, h), Y, X\Y, X, Sem2, Sem1, app(Sem1, Sem2)).
binary_rule(comp(1, f, h), X/Y, Y/Z, X/Z, Sem1, Sem2, lam(A, app(Sem1, app(Sem2, A)))).
binary_rule(comp(1, b, h), Y\Z, X\Y, X\Z, Sem2, Sem1, lam(A, app(Sem1, app(Sem2, A)))).
binary_rule(comp(1, f, x), X/Y, Y\Z, X\Z, Sem1, Sem2, lam(A, app(Sem1, app(Sem2, A)))).
binary_rule(comp(1, b, x), Y/Z, X\Y, X/Z, Sem2, Sem1, lam(A, app(Sem1, app(Sem2, A)))).
binary_rule(comp(2, f, h), X/Y, (Y/Z2)/Z1, (X/Z2)/Z1, Sem1, Sem2, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).
binary_rule(comp(2, b, h), (Y\Z2)\Z1, X\Y, (X\Z2)\Z1, Sem2, Sem1, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).
binary_rule(comp(2, f, x), X/Y, (Y\Z2)\Z1, (X\Z2)\Z1, Sem1, Sem2, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).
binary_rule(comp(2, b, x), (Y/Z2)/Z1, X\Y, (X/Z2)/Z1, Sem2, Sem1, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).
binary_rule(comp(2, f, x), X/Y, (Y\Z2)/Z1, (X\Z2)/Z1, Sem1, Sem2, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).
binary_rule(comp(2, b, x), (Y/Z2)\Z1, X\Y, (X/Z2)\Z1, Sem2, Sem1, lam(A, lam(B, app(Sem1, app(app(Sem2, A), B))))).

%%	normal_form(+LRule, +RRule, +Rule)
%
%	True iff Rule can be used on the output of LRule on the left and RRule
%	on the right according to the normal form constraints of Hockenmaier
%	and Bisk (2010).
normal_form(PrimaryInputRule, SecondaryInputRule, comp(N, f, Crossed)) :-
  \+ violates_nfc1(PrimaryInputRule, comp(N, f, Crossed)),
  \+ violates_nfc2(PrimaryInputRule, comp(N, f, Crossed)),
  \+ violates_nfc3(SecondaryInputRule, comp(N, f, Crossed)),
  \+ violates_nfc4(PrimaryInputRule, SecondaryInputRule, comp(N, f, Crossed)),
  \+ violates_nfc5(PrimaryInputRule, comp(N, f, Crossed)).
normal_form(SecondaryInputRule, PrimaryInputRule, comp(N, b, Crossed)) :-
  \+ violates_nfc1(PrimaryInputRule, comp(N, b, Crossed)),
  \+ violates_nfc2(PrimaryInputRule, comp(N, b, Crossed)),
  \+ violates_nfc3(SecondaryInputRule, comp(N, b, Crossed)),
  \+ violates_nfc4(PrimaryInputRule, SecondaryInputRule, comp(N, b, Crossed)),
  \+ violates_nfc5(PrimaryInputRule, comp(N, b, Crossed)).

% IIUC, the composition rules that are subject to Hockenmaier and Bisk's
% constraints are: 1) application, 2) harmonic composition (with n=1), 3) any
% generalized composition (with n>1).
is_restricted(comp(0, Dir, h), 0, Dir) :-
  !.
is_restricted(comp(1, Dir, h), 1, Dir) :-
  !.
is_restricted(comp(N, Dir, _), N, Dir) :-
  N > 1.

violates_nfc1(PrimaryInputRule, Rule) :-
  is_restricted(PrimaryInputRule, M, Dir),
  M >= 1,
  is_restricted(Rule, N, Dir),
  N =< 1.

violates_nfc2(PrimaryInputRule, Rule) :-
  is_restricted(PrimaryInputRule, 1, Dir),
  is_restricted(Rule, N, Dir),
  N >= 1.

violates_nfc3(SecondaryInputRule, Rule) :-
  is_restricted(SecondaryInputRule, M, Dir),
  is_restricted(Rule, N, Dir),
  N > M.

violates_nfc4(ftr, SecondaryInputRule, Rule) :-
  is_restricted(Rule, N, f),
  N > 0,
  is_restricted(SecondaryInputRule, M, b),
  M > N.
violates_nfc4(btr, SecondaryInputRule, Rule) :-
  is_restricted(Rule, N, b),
  N > 0,
  is_restricted(SecondaryInputRule, M, f),
  M > N.
  
violates_nfc5(ftr, comp(0, f, h)).
violates_nfc5(btr, comp(0, b, h)).
