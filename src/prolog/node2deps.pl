:- module(node2deps, [
    main/0]).

:- use_module(catobj, [
    co2cat/2,
    co_res_arg/3,
    cos_tops_deps/3,
    co_top/2,
    is_modifier_co/1]).
:- use_module(ccgbank, [
    cat//1]).
:- use_module(node, [
    node_co/2,
    node_rule/2,
    node_sem/2,
    node_from_to/3,
    token_in_node/2,
    typechanger_in_node/2]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    substitute_sub_term/3,
    term_in_file/3,
    write_clause/1,
    write_term_vars/2]).

main :-
  argv([NodeFile]),
  findall(Term, term_in_file(Term, NodeFile, [module(slashes)]), Terms),
  node2deps(Terms, 1),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l node2deps -g main NODEFILE~n', []),
  halt(1).

node2deps([], _) :-
  nl.
node2deps([node(SID, Node)|Rest], SID) :-
  !,
  node2deps(Node),
  node2deps(Rest, SID).
node2deps(Rest, SID) :-
  nl,
  NewSID is SID + 1,
  node2deps(Rest, NewSID).

node2deps(Node) :-
  findall(Token, token_in_node(Token, Node), Tokens),
  findall(TC, typechanger_in_node(TC, Node), TCs),
  maplist(node_co, Tokens, TokenCOs),
  maplist(tc_newco_oldco, TCs, TCNewCOs, TCOldCOs),
  maplist(co_top, TCOldCOs, TCOldTopCOs),
  append(TokenCOs, TCNewCOs, COs),
  cos_tops_deps(COs, TopCOs, Deps0),
  length(TokenCOs, L),
  length(TokenTopCOs, L),
  append(TokenTopCOs, TCTopCOs, TopCOs),
  maplist(tok4tc(TCTopCOs, TCOldTopCOs), Deps0, Deps),
  %write(Tokens),nl,
  %write(TCs),nl,
  %write(TokenTopCOs),nl,
  %write(TCTopCOs),nl,
  %write(TCOldCOs),nl,
  %write(TCOldTopCOs),nl,
  %write(Deps0),nl,
  %write(Deps),nl,
  %write(Tokens),nl,
  maplist(write_dep(Tokens, TokenTopCOs, Deps), Tokens, TokenTopCOs).

tc_newco_oldco(NewCO-OldCO, NewCO, OldCO).

tok4tc(TCTopCOs, TCOldCOs, dep(Dep0, Head0, Label), dep(Dep, Head, Label)) :-
  tok4tc_(TCTopCOs, TCOldCOs, Dep0, Dep),
  tok4tc_(TCTopCOs, TCOldCOs, Head0, Head).

tok4tc_(TCTopCOs, TCOldCOs, CO0, CO) :-
  nth1(N, TCTopCOs, CO0),
  nth1(N, TCOldCOs, CO1),
  !,
  tok4tc_(TCTopCOs, TCOldCOs, CO1, CO).
tok4tc_(_, _, CO, CO).

write_dep(Tokens, TopCOs, Deps, DToken, TopCO) :-
  member(dep(TopCO, Head, Label), Deps),
  !,
  nth1(N, TopCOs, Head),
  nth1(N, Tokens, HToken),
  node_from_to(DToken, DFrom, DTo),
  node_rule(DToken, t(DForm, _)),
  node_co(DToken, DCO),
  co2cat(DCO, DCat),
  phrase(cat(DCat), DCatCodes),
  node_from_to(HToken, HFrom, HTo),
  node_rule(HToken, t(HForm, _)),
  node_co(HToken, HCO),
  co2cat(HCO, HCat),
  phrase(cat(HCat), HCatCodes),
  phrase(cat(Label), LabelCodes),
  format('~w\t~w\t~w\t~s\t~w\t~w\t~w\t~s\t~s~n', [DFrom, DTo, DForm, DCatCodes, HFrom, HTo, HForm, HCatCodes, LabelCodes]).
write_dep(_, _, _, DToken, _) :-
  node_from_to(DToken, DFrom, DTo),
  node_rule(DToken, t(DForm, _)),
  node_co(DToken, DCO),
  co2cat(DCO, DCat),
  phrase(cat(DCat), DCatCodes),
  format('~w\t~w\t~w\t~s~n', [DFrom, DTo, DForm, DCatCodes]).
