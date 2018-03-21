:- module(node2deps, [
    main/0]).

:- use_module(boxer(betaConversionDRT), [
    betaConvert/2]).
:- use_module(catobj, [
    co2cat/2]).
:- use_module(ccgbank, [
    cat//1]).
:- use_module(node, [
    node_co/2,
    node_rule/2,
    node_sem/2,
    node_from_to/3,
    token_in_node/2]).
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

node2deps(Node) :-
  node_sem(Node, Sem0),
  beta_convert(Sem0, Sem),
  sem_head_deps(Sem, _, Deps),
  sort(Deps, SortedDeps),
  forall(
      ( member((DFrom-DTo)-(HFrom-HTo), SortedDeps)
      ),
      ( token_in_node(DToken, Node),
        node_from_to(DToken, DFrom, DTo),
	node_rule(DToken, t(DForm, _)),
	node_co(DToken, DCO),
	co2cat(DCO, DCat),
	phrase(cat(DCat), DCatCodes),
	token_in_node(HToken, Node),
	node_from_to(HToken, HFrom, HTo),
	node_rule(HToken, t(HForm, _)),
	node_co(HToken, HCO),
	co2cat(HCO, HCat),
	phrase(cat(HCat), HCatCodes),
	format('~w\t~w\t~w\t~s\t~w\t~w\t~w\t~s~n', [DFrom, DTo, DForm, DCatCodes, HFrom, HTo, HForm, HCatCodes])
      ) ).

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
