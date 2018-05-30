:- module(node2deps, [
    main/0]).

:- use_module(catobj, [
    co2cat/2,
    co_res_arg/3,
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
    write_clause/3]).

main :-
  argv([NodeFile, Style]),
  assertion(member(Style, [plain, mod, det, alpino])),
  forall(
      ( term_in_file(node(_, Node), NodeFile, [module(slashes)])
      ),
      ( node2deps(Node, Style, Deps),
        check_deps(Node, Deps),
        write_deps(Node, Deps)
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l node2deps -g main NODEFILE STYLE~n', []),
  halt(1).

check_deps(Node, Deps) :-
  aggregate_all(count, token_in_node(_, Node), TokenCount),
  length(Deps, DepCount),
  assertion(DepCount is TokenCount - 1).

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
        ;  write('\t0\t_\t_\tROOT')
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

node2deps(Node, Style, Deps) :-
  findall(Dep,
      ( original_co_in_node(CO, Node), 
        co2cat(CO, Cat),
        depdirs(Style, Cat, Dirs),
        co_dirs_deps_tok_target(Style, Node, CO, Dirs, Deps, Tok, Tok, _),
        member(Dep, Deps)
      ), Deps).

%%      depdirs(+Style, +Cat, -Dirs)
%
%       For each argument of Cat, computes whether or not the dependency
%       between functor and argument should be inverted. This depends on the
%       Style:
%
%           * =plain= - functors are the heads of their arguments (like Koller
%             and Kuhlmann, 2009)
%           * =mod= - head-dependent relationship is inverted between modifiers
%             and their arguments
%           * =det= - like =mod=, but head-dependent relationship is also
%             inverted for the arguments of =|np/n|= and =|np/(n/pp)|=
%           * =alpino= like =det=, but conjunctions head both conjuncts
%
%      Dirs is bound to a list of terms =normal= or =inverted=, corresponding
%      to the arguments.

depdirs(plain, Cat, Dirs) :-
  depdirs(no, no, no, Cat, Dirs).
depdirs(mod, Cat, Dirs) :-
  depdirs(yes, no, no, Cat, Dirs).
depdirs(det, Cat, Dirs) :-
  depdirs(yes, no, yes, Cat, Dirs).
depdirs(alpino, Cat, Dirs) :-
  depdirs(yes, yes, yes, Cat, Dirs).

%%      depdirs(+Mod, +Coord, +Det, +Cat, -Dirs)
%
%       Computes the directions of functor-argument dependencies, depending on
%       various features:
%
%           * Mod is one of =yes=, =no= - whether or not to invert
%             modifier-argument dependencies
%           * Coord is one of =no=, =alpino= - if =alpino=, conjunctions head
%             both conjuncts.
%           * Det is one of =no=, =yes= - whether or not to treat determiners
%             as dependents of their arguments.
%
%       The last two arguments are the input category (Cat) and the list of
%       dependency directions (=normal= or =inverted=) for each argument.

depdirs(_, _, yes, np/n, [inverted]) :-
  !.
depdirs(_, _, yes, np/(n/pp), [inverted]) :-
  !.
depdirs(Mod, Coord, _, Cat, Dirs) :-
  depdirs(Mod, Coord, Cat, Dirs).

%%      depdirs(+Mod, +Coord, +Cat, -Dirs)
%
%       Like =|depdir/5|=, but computes Dirs from Cat recursively after the
%       non-recursive rules (such as for Det) have applied.
%
%       Arguments:
%
%           * Cat is the category
%           * Dirs is a list of the directions (=normal= or =inverted=) of all
%             arguments.
%

% Treat coordination Alpino-style if Coord=alpino
depdirs(yes, alpino, (X\X)/X, [noninv, noninv|Dirs]) :-
  !,
  depdirs(yes, alpino, X, Dirs).
% Otherwise, treat modifier as dependent if Mod=yes
depdirs(yes, Coord, X/X, [inverted|Dirs]) :-
  !,
  depdirs(yes, Coord, X, Dirs).
depdirs(yes, Coord, X\X, [inverted|Dirs]) :-
  !,
  depdirs(yes, Coord, X, Dirs).
% Otherwise, treat argument as dependent
depdirs(Mod, Coord, Res/_, [normal|Dirs]) :-
  !,
  depdirs(Mod, Coord, Res, Dirs).
depdirs(Mod, Coord, Res\_, [normal|Dirs]) :-
  !,
  depdirs(Mod, Coord, Res, Dirs).
% Base case: no argument
depdirs(_, _, _, []).

%%      co_dirs_deps_tok_target(+Style, +Node, +CO, +Dirs, -Deps, -Tok, +Target0, -Target)
%
%       Given a style Style, category object CO, the argument dependency
%       directions Dirs and a node object Node (representing the entire
%       derivation), computes the dependencies (Deps), the token that
%       corresponds to the CO for the purpose of CCG predicate-argument
%       structure (Tok) and the token that corresponds to the top CO of
%       CO for the purpose of dependency graphs with inversion of
%       head-modifier dependencies.
%
%       Target0 is the token on which arguments will depend until
%       modification is encountered; bind this to Tok by default.
%
co_dirs_deps_tok_target(Style, Node, CO, [Dir|Dirs], [Dep|Deps], Tok, Target0, Target) :-
  % Case 1: functional CO where the argument has an origin
  co_res_arg(CO, Res, Arg),
  co_node_origin(Arg, Node, ArgOrigin),
  co2cat(ArgOrigin, ArgOriginCat),
  depdirs(Style, ArgOriginCat, ArgDirs),
  co_dirs_deps_tok_target(Style, Node, ArgOrigin, ArgDirs, _, ArgTok, ArgTok, ArgTarget),
  !,
  co2cat(CO, Cat),
  (  Dir = inverted
  -> Dep = dep(Target0, ArgTarget, Cat),
     co_dirs_deps_tok_target(Style, Node, Res, Dirs, Deps, Tok, ArgTarget, Target)
  ;  Dep = dep(ArgTarget, Target0, Cat),
     co_dirs_deps_tok_target(Style, Node, Res, Dirs, Deps, Tok, Target0, Target)
  ).
co_dirs_deps_tok_target(Style, Node, CO, [_|Dirs], Deps, Tok, Target0, Target) :-
  % Case 2: functional CO where the argument has no origin
  co_res_arg(CO, Res, _),
  !,
  co_dirs_deps_tok_target(Style, Node, Res, Dirs, Deps, Tok, Target0, Target).
co_dirs_deps_tok_target(_, Node, CO, _, [], Tok, Target, Target) :-
  % Case 2: atomic CO, lexical origin
  token_in_node(Tok, Node),
  node_co(Tok, TokCO),
  co_top(TokCO, CO),
  !.
co_dirs_deps_tok_target(Style, Node, CO, _, [], Tok, Target0, Target) :-
  % Case 3: atomic CO or with phantom argument, tc origin
  typechanger_in_node(New-Old, Node),
  co_top(New, CO),
  co_node_origin(Old, Node, CO2),
  co2cat(CO2, Cat2),
  depdirs(Style, Cat2, Dirs2),
  co_dirs_deps_tok_target(Style, Node, CO2, Dirs2, _, Tok, Target0, Target).

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

% Modifiers
inverts_dependency(X/X).
inverts_dependency(X\X).
% Conjunctions and adpositions
% Problem: S[dcl] can be an argument to S[dcl]. Special treatment for punctuation?
%inverts_dependency((X\X)/_).
%inverts_dependency((X/X)/_).
%inverts_dependency((X\X)\_).
%inverts_dependency((X\X)\_).
% Determiners
inverts_dependency(np/n).
inverts_dependency(np/(n/pp)).
% Complementizers
%inverts_dependency((s:_\np)/(s:_\np)).
