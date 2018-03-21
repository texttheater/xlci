:- module(node2deps, [
    main/0]).

:- use_module(boxer(betaConversionDRT), [
    betaConvert/2]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    substitute_sub_term/3,
    term_in_file/3,
    write_term_vars/2]).

main :-
  argv([NodeFile]),
  findall(Term, term_in_file(Term, NodeFile, [module(slashes)]), Terms),
  node2deps(Terms, 1),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l node2deps -g main NODEFILE~n', []),
  halt(1).

node2deps([], _).
node2deps([node(SID, Node)|Rest], SID) :-
  !,
  node2deps(Node),
  node2deps(Rest, SID).
node2deps(Rest, SID) :-
  nl,
  NewSID is SID + 1,
  node2deps(Rest, NewSID).

node2deps(node(_, Sem0, _, _Children)) :-
  beta_convert(Sem0, Sem),
  sem_head_deps(Sem, _, Deps),
  write_term_vars(Deps, [quoted(true)]).

% TODO invert for modifiers? How?
sem_head_deps(app(nil, B), BHead, BDeps) :-
  !,
  sem_head_deps(B, BHead, BDeps).
sem_head_deps(app(A, B), AHead, Deps) :-
  !,
  sem_head_deps(A, AHead, ADeps),
  (  var(B)
  -> ADeps = Deps
  ;  sem_head_deps(B, BHead, BDeps),
     append([[BHead-AHead], ADeps, BDeps], Deps)
  ).
sem_head_deps(lam(_, Scope), Head, Deps) :-
  !,
  sem_head_deps(Scope, Head, Deps).
sem_head_deps(Sem, Sem, []).

beta_convert(Sem0, Sem) :-
  substitute_sub_term(pair_atom, Sem0, Sem1),
  betaConvert(Sem1, Sem2),
  substitute_sub_term(atom_pair, Sem2, Sem).

pair_atom(Pair, Atom) :-
  nonvar(Pair),
  Pair = _-_,
  term_to_atom(Pair, Atom).

atom_pair(Atom, From-To) :-
  atom(Atom),
  read_term_from_atom(Atom, From-To, []).
