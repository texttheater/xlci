:- module(node, [
    node_co/2,
    node_from_to/3,
    node_rule/2,
    node_sem/2,
    pp_node/1,
    token_in_node/2]).

/** <module> Utilities for our =|node/4|= CCG derivation representation format
*/

:- use_module(slashes).
:- use_module(util, [
    print_indented/3]).

node_co(node(CO, _, _, _), CO).

node_sem(node(_, Sem, _, _), Sem).

node_rule(node(_, _, Rule, _), Rule).

node_from_to(node(_, _, t(_, Atts), []), From, To) :-
  !,
  member(from:From, Atts),
  member(to:To, Atts).
node_from_to(node(_, _, _, Children), From, To) :-
  Children = [First|_],
  node_from_to(First, From, _),
  last(Children, Last),
  node_from_to(Last, _, To).

token_in_node(node(Cat, Sem, t(Form, Atts), []), node(Cat, Sem, t(Form, Atts), [])) :-
  !.
token_in_node(Token, node(_, _, _, Children)) :-
  member(Child, Children),
  token_in_node(Token, Child).

pp_node(Node) :-
  print_indented(Node, [node(_, _, t(_, _), _), lam(_, _), app(_, _), _\_, _/_, co(_, _, _), comp(_, _), comp(_, _, _), tc(_)], [module(slashes), fullstop(true)]).

