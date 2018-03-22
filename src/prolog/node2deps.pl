:- module(node2deps, [
    main/0]).

:- use_module(boxer(betaConversionDRT), [
    betaConvert/2]).
:- use_module(catobj, [
    co2cat/2,
    co_res_arg/3,
    is_modifier_co/1]).
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

% TODO add argument to record dependencies
node_head_stack_tr_deps(node(_, _, tc(_), [D]), Head, [], no, Deps) :-
  node_head_stack_tr(D, Head, [], no, Deps). % TODO nonempty stack or type raising possible?
node_head_stack_tr_deps(node(CO, Sem, t(Form, Atts), []), node(CO, Sem, t(Form, Atts), []), [], no, []).
node_head_stack_tr_deps(node(_, _, TR, [D]), Head, [], yes, Deps) :-
  member(TR, [ftr, btr]),
  node_head_stack_tr_deps(D, Head, [], no, Deps). % TODO nonempty stack or type raising possible?
node_head_stack_tr_deps(node(CO, _, comp(N, Dir), Daughters), Head, Stack, no, Deps) :-
  dir_daughters_d1_d2(Dir, Daughters, D1, D2),
  node_head_stack_tr_deps(D1, Head1, Stack1, TR1, Deps1),
  node_head_stack_tr_deps(D2, Head2, Stack2, _, Deps2),
  (  TR1 = yes
  -> ( co_res_arg(CO, _, ArgCO),
       (  is_modifier_cat(ArgCO)
       -> Head = Head1,
	  Dep = Head2-Head1
       ;  Head = Head2,
	  Dep = Head1-Head2
       )
     )
  ;  (  is_modifier_cat(CO)
     -> Head = Head2,
        Dep = Head1-Head2
     ;  Head = Head1,
        Dep = Head2-Head1
     )
  ),
  node_co(D2, CO2),
  n_co2_head2_stack2_stackargs(N, CO2, Head2, Stack2, StackArgs),
  append(StackArgs, Stack1, Stack),
  append([Deps1, [Dep], Deps2], Deps).

dir_daughters_d1_d2(f, [D1, D2], D1, D2).
dir_daughters_d1_d2(b, [D2, D1], D1, D2).

n_co2_head2_stack2_stackargs(0, _, _, _, []) :-
  !.
n_co2_head2_stack2_stackargs(N, CO2, Head2, [StackArg|Stack2], [StackArg|StackArgs]) :-
  M is N - 1,
  n_co2_head2_stack2_stackargs(M, CO2, Head2, Stack2, StackArgs).
n_co2_head2_stack2_stackargs(N, CO2, Head2, [], [Head2-Modifier|StackArgs]) :-
  M is N - 1,
  (  is_modifier_co(CO2)
  -> Modifier = yes
  ;  Modifier = no
  ),
  co_res_arg(CO2, ResCO, _),
  n_co2_head2_stack2_stackargs(M, ResCO, Head2, [], StackArgs).

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
