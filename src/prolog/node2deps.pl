:- module(node2deps, [
    main/0]).

:- use_module(catobj, [
    co2cat/2,
    co_res_arg/3,
    is_modifier_co/1,
    co_top/2]).
:- use_module(ccgbank, [
    cat//1]).
:- use_module(node, [
    node_co/2,
    node_rule/2,
    token_in_node/2,
    typechanger_in_node/2
    ]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    term_in_file/3,
    write_clause/1]).

main :-
  argv([NodeFile]),
  forall(
      ( term_in_file(node(_, Node), NodeFile, [module(slashes)])
      ),
      ( node2deps(Node, Deps),
        write_deps(Node, Deps)
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l node2deps -g main NODEFILE~n', []),
  halt(1).

write_deps(Node, Deps) :-
  forall(
      ( token_in_node(Token, Node)
      ),
      ( write_token(Token),
        (  member(dep(Token, Head, Label), Deps)
        -> write('\t'),
           write_token(Head),
           write('\t'),
           write_label(Label)
        ;  true
        ),
        nl
      ) ),
  nl.

write_token(Token) :-
  node_co(Token, CO),
  co2cat(CO, Cat),
  phrase(cat(Cat), CatCodes),
  node_rule(Token, t(Form, Atts)),
  atts_tokid(Atts, TokID),
  format('~w\t~w\t~s', [TokID, Form, CatCodes]).

write_label(Cat) :-
  phrase(cat(Cat), CatCodes),
  format('~s', [CatCodes]).

atts_tokid(Atts, TokNum) :-
  member(toknum:TokNum, Atts).
atts_tokid(Atts, (From, To)) :-
  member(from:From, Atts),
  member(to:To, Atts).

node2deps(Node, Deps) :-
  findall(Dep,
      ( original_co_in_node(CO, Node), 
        co_node_deps_tok_target(CO, Node, Deps, Tok, Tok, _),
        member(Dep, Deps)
      ), Deps).

%%      co_node_deps_tok_target(+CO, +Node, -Deps, -Tok, +Target0, -Target)
%
%       Given a category object and a node object (representing the entire
%       derivation), computes the dependencies (Deps), the token that
%       corresponds to the CO for the purpose of CCG predicate-argument
%       structure (Tok) and the token that corresponds to the top CO of
%       CO for the purpose of dependency graphs with inversion of
%       head-modifier dependencies.
%
%       Target0 is the token on which arguments will depend until
%       modification is encountered; bind this to Tok by default.
co_node_deps_tok_target(CO, Node, [Dep|Deps], Tok, Target0, Target) :-
  % Case 1: functional CO where the argument exists
  co_res_arg(CO, Res, Arg),
  co_node_origin(Arg, Node, ArgOrigin),
  co_node_deps_tok_target(ArgOrigin, Node, _, ArgTok, ArgTok, ArgTarget),
  !,
  co2cat(CO, Cat),
  (  is_modifier_co(CO)
  -> Dep = dep(Target0, ArgTarget, Cat),
     co_node_deps_tok_target(Res, Node, Deps, Tok, ArgTarget, Target)
  ;  Dep = dep(ArgTarget, Target0, Cat),
     co_node_deps_tok_target(Res, Node, Deps, Tok, Target0, Target)
  ).
co_node_deps_tok_target(CO, Node, [], Tok, Target, Target) :-
  % Case 2: atomic CO or with phantom argument, lexical origin
  co_top(CO, Top),
  token_in_node(Tok, Node),
  node_co(Tok, TokCO),
  co_top(TokCO, Top),
  !.
co_node_deps_tok_target(CO, Node, [], Tok, Target0, Target) :-
  % Case 3: atomic CO or with phantom argument, tc origin
  typechanger_in_node(New-Old, Node),
  co_top(New, CO),
  co_node_origin(Old, Node, CO2),
  co_node_deps_tok_target(CO2, Node, _, Tok, Target0, Target).

% "Original" COs are those that are introduced by tokens or type changing.
original_co_in_node(CO, Node) :-
  token_in_node(Token, Node),
  node_co(Token, CO).
original_co_in_node(CO, Node) :-
  typechanger_in_node(CO-_, Node).

% Maps any CO to the corresponding original CO
co_node_origin(CO, Node, Origin) :-
  original_co_in_node(Origin, Node),
  co_top(CO, Top),
  co_top(Origin, Top).
