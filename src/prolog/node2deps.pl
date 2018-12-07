:- module(node2deps, [
    main/0]).

:- use_module(cat, [
    strip_features/2]).
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
    substitute_sub_term/3,
    term_in_file/3,
    write_clause/2]).

main :-
  argv([NodeFile, Style]),
  assertion(member(Style, [plain, mod, det, pascal_arabic, pascal_basque, pascal_czech, pascal_danish, pascal_dutch, pascal_english, pascal_portuguese, pascal_slovene, pascal_swedish, ud])),
  findall(Node, term_in_file(Node, NodeFile, [module(slashes)]), Nodes),
  process(Style, 1, Nodes),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l node2deps -g main NODEFILE STYLE~n', []),
  halt(1).

process(Style, I, [node(I, Node)|Nodes]) :-
  !,
  node2deps(Node, Style, Deps),
  check_deps(Node, Deps),
  write_deps(Node, Deps),
  process(Style, I, Nodes).
process(_, _, []) :-
  nl.
process(Style, I, Nodes) :-
  nl,
  J is I + 1,
  process(Style, J, Nodes).

check_deps(Node, Deps) :-
  aggregate_all(count, token_in_node(_, Node), TokenCount),
  length(Deps, DepCount),
  Expected is TokenCount - 1,
  (  DepCount = Expected
  -> true
  ;  format(user_error, 'WARNING: ~w tokens, expected ~w dependencies but got ~w~n', [TokenCount, Expected, DepCount])
  ).

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
      ) ).

write_token(Token) :-
  node_co(Token, CO),
  co2cat(CO, Cat),
  phrase(cat(Cat), CatCodes),
  node_rule(Token, t(Form, Atts)),
  (  atts_tokid(Atts, TokID)
  -> true
  ;  Form = [TokID|_] % HACK: token ID list, can be multiword, we take the first
  ),
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
        depdirs(Style, Cat, Dirs),%phrase(cat(Cat), CatCodes),format(user_error, '~s ~w~n', [CatCodes, Dirs]),
        co_dirs_deps_tok_target(Style, Node, CO, Dirs, Deps, Tok, Tok, _),
        member(Dep, Deps)
      ), Deps0),
      %maplist(write_clause(user_error), Deps0),
      %format(user_error, '~~~n', []),
  fix_coord(Style, Deps0, Deps).
  %maplist(write_clause(user_error), Deps).

% The coordination dependency scheme used for Spanish, German and Portuguese
% cannot be expressed as a special case of our conversion algorithm, so we
% convert it to UD-style first and then fix it here.
fix_coord(Style, Deps0, Deps) :-
  member(Style, [pascal_german, pascal_portuguese, pascal_spanish]),
  member(dep(X1, X2, X\X), Deps0),
  select(dep(X2, CC, (X\X)/X), Deps0, Deps1),
  !,
  fix_coord(Style, [dep(X1, CC, (X\X)/X)|Deps1], Deps).
fix_coord(_, Deps, Deps).

%%      depdirs(+Style, +Cat, -Dirs)
%
%       For each argument of Cat, computes whether or not the dependency
%       between functor and argument should be inverted. This depends on the
%       Style:
%
%           * =plain= - functors are the heads of their arguments (like Koller
%             and Kuhlmann, 2009)
%           * =mod= - head-dependent relationship is inverted between modifiers
%             and their arguments (standard)
%           * =det= - like =mod=, but head-dependent relationship is also
%             inverted for the arguments of =|np/n|= and =|np/(n/pp)|=
%           * =pascal_arabic=, =pascal_basque=, =pascal_czech=,
%             =pascal_danish=, =pascal_dutch=, =pascal_portuguese=,
%             =pascal_slovene=, =pascal_swedish= - attempts to match the
%             dependency schemes of the various PASCAL treebanks
%           * =ud= - targetting Universal Dependencies 
%
%      Dirs is bound to a list of terms =normal= or =inverted=, corresponding
%      to the arguments.

depdirs(plain, Cat, Dirs) :-
  depdirs(no, x1_cc___cc_x2, no, no, no, no, Cat, Dirs).
depdirs(mod, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_cc___cc_x2, no, no, no, no, Cat, Dirs).
depdirs(det, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_cc___cc_x2, yes, no, no, no, Cat, Dirs).
depdirs(pascal_arabic, Cat, Dirs) :-
  depdirs(feat_sensitive, cc_x1___cc_x2, yes, no, no, no, Cat, Dirs).
depdirs(pascal_basque, Cat, Dirs) :-
  depdirs(feat_sensitive, cc_x1___cc_x2, yes, no, no, no, Cat, Dirs).
depdirs(pascal_czech, Cat, Dirs) :-
  depdirs(feat_sensitive, cc_x1___cc_x2, yes, no, no, yes, Cat, Dirs).
depdirs(pascal_danish, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_cc___cc_x2, no, no, no, no, Cat, Dirs).
depdirs(pascal_english, Cat, Dirs) :-
  depdirs(feat_sensitive, cc_x1___cc_x2, yes, yes, no, no, Cat, Dirs).
depdirs(pascal_dutch, Cat, Dirs) :-
  depdirs(feat_sensitive, cc_x1___cc_x2, yes, no, no, no, Cat, Dirs).
depdirs(pascal_portuguese, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_x2___x2_cc, yes, yes, no, no, Cat, Dirs). % coord will be fixed by fix_coord/3
depdirs(pascal_slovene, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_x2___x2_cc, yes, yes, yes, yes, Cat, Dirs).
depdirs(pascal_swedish, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_x2___x2_cc, yes, yes, no, no, Cat, Dirs).
depdirs(ud, Cat, Dirs) :-
  depdirs(feat_sensitive, x1_x2___x2_cc, yes, yes, yes, yes, Cat, Dirs).
% TODO get rid of feat_insensitive?

%%      depdirs(+Mod, +Coord, +Det, +Subo, +Prep, +Aux, +Cat, -Dirs)
%
%       Computes the directions of functor-argument dependencies, depending on
%       various features:
%
%           * Mod is one of =no=, =feat_sensitive=, =feat_insensitive= - =no=
%             treats modifiers as heads of their arguments. =feat_sensitive=
%             treats modifiers as dependents of their arguments.
%             =feat_insensitive= likewise, but ignores features when
%             determining whether or not a category is a modifier category.
%           * Coord specifies the dependencies created by conjunction.
%           * Det is one of =no=, =yes= - whether or not to treat determiners
%             as dependents of their arguments.
%           * Subo is one of =no=, =yes= - whether or not to treat
%             subordinating words (subordinating conjunctions, complementizers,
%             and relative pronouns) as dependent of their argument clauses.
%           * Prep is one of =no=, =yes= - whether or not to treat prepostions
%             as dependents of their arguments.
%           * Aux is one of =no=, =yes= - whether or not to treat auxiliary
%             verbs as dependents of their arguments.
%
%       The last two arguments are the input category (Cat) and the list of
%       dependency directions (=normal= or =inverted=) for each argument.

% Treat coordination like cc_x1___cc_x2
depdirs(Mod, cc_x1___cc_x2, Det, Subo, Prep, Aux, (X\X)/X, [normal, normal|Dirs]) :-
  !,
  depdirs(Mod, cc_x1___cc_x2, Det, Subo, Prep, Aux, X, Dirs).
% Treat coordination like x1_x2___x2_cc
depdirs(Mod, x1_x2___x2_cc, Det, Subo, Prep, Aux, (X\X)/X, [inverted, inverted|Dirs]) :-
  !,
  depdirs(Mod, cc_x1___x2_cc, Det, Subo, Prep, Aux, X, Dirs).
% Treat modifier as dependent
depdirs(Mod, Coord, Det, Subo, Prep, Aux, X/Y, [inverted|Dirs]) :-
  is_modifier_category(Mod, X/Y),
  !,
  depdirs(feat_sensitive, Coord, Det, Subo, Prep, Aux, X, Dirs).
depdirs(Mod, Coord, Det, Subo, Prep, Aux, X\Y, [inverted|Dirs]) :-
  is_modifier_category(Mod, X\Y),
  !,
  depdirs(feat_sensitive, Coord, Det, Subo, Prep, Aux, X, Dirs).
% Treat determiners as dependent if Det=yes
depdirs(_, _, yes, _, _, _, np/n, [inverted]) :-
  !.
depdirs(_, _, yes, _, _, _, np/(n/pp), [inverted]) :-
  !.
% Treat subordinating conjunctions, complementizers, and relative pronouns as dependent if Subo=yes
depdirs(Mod, Coord, Det, yes, Prep, Aux, X/Y, [inverted|Dirs]) :-
  is_subordinating_category(X/Y),
  !,
  depdirs(Mod, Coord, Det, yes, Prep, Aux, X, Dirs).
depdirs(Mod, Coord, Det, yes, Prep, Aux, X\Y, [inverted|Dirs]) :-
  is_subordinating_category(X\Y),
  !,
  depdirs(Mod, Coord, Det, yes, Prep, Aux, X, Dirs).
% Treat prepositions as dependent if Prep=yes
depdirs(Mod, Coord, Det, Subo, yes, Aux, X/Y, [inverted|Dirs]) :-
  is_adposition(X/Y),
  !,
  depdirs(Mod, Coord, Det, Subo, yes, Aux, X, Dirs).
depdirs(Mod, Coord, Det, Subo, yes, Aux, X\Y, [inverted|Dirs]) :-
  is_adposition(X\Y),
  !,
  depdirs(Mod, Coord, Det, Subo, yes, Aux, X, Dirs).
% Treat auxiliaries as dependent if Aux=yes
depdirs(Mod, Coord, Det, Subo, Prep, yes, X/Y, [inverted|Dirs]) :-
  is_aux(X/Y),
  !,
  depdirs(Mod, Coord, Det, Subo, Prep, yes, X, Dirs).
depdirs(Mod, Coord, Det, Subo, Prep, yes, X\Y, [inverted|Dirs]) :-
  is_aux(X\Y),
  !,
  depdirs(Mod, Coord, Det, Subo, Prep, yes, X, Dirs).
% Otherwise, treat argument as dependent
depdirs(Mod, Coord, Det, Subo, Prep, Aux, Res/_, [normal|Dirs]) :-
  !,
  depdirs(Mod, Coord, Det, Subo, Prep, Aux, Res, Dirs).
depdirs(Mod, Coord, Det, Subo, Prep, Aux, Res\_, [normal|Dirs]) :-
  !,
  depdirs(Mod, Coord, Det, Subo, Prep, Aux, Res, Dirs).
% Base case: no argument
depdirs(_, _, _, _, _, _, _, []).

is_modifier_category(feat_sensitive, X/X).
is_modifier_category(feat_sensitive, X\X).
is_modifier_category(feat_insensitive, A/B) :-
  strip_features(A, X),
  strip_features(B, X).
is_modifier_category(feat_insensitive, A\B) :-
  strip_features(A, X),
  strip_features(B, X).

is_subordinating_category(Cat) :-
  is_subordinating_conjunction(Cat).
is_subordinating_category(Cat) :-
  is_complementizer(Cat).
is_subordinating_category(Cat) :-
  is_relative_pronoun(Cat).

is_subordinating_conjunction(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s\s, s/s, (s\np)\(s\np), (s\np)/(s\np), (s/np)\(s/np), (s/np)/(s/np)]),
  member(Y, [s:dcl, s:to, s:ng\np, s:ng/np]).

is_complementizer(Cat) :-
  member(Cat, [s:em/s:dcl, s:em\s:dcl, (s:to\np)/(s:b\np), (s:to\np)\(s:b\np), (s:to/np)/(s:b/np), (s:to/np)\(s:b/np)]).

is_relative_pronoun(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [n\n, n/n, np\np, np/np]),
  member(Y, [s:dcl/np, s:dcl\np]).

is_adposition(Cat) :-
  member(Cat, [PP/np, PP\np]),
  member(PP, [pp, n\n, n/n, np\np, np/np, s\s, s/s, (s\np)\(s\np), (s\np)/(s\np), (s/np)\(s/np), (s/np)/(s/np)]).

is_aux(Cat) :-
  member(Cat, [X/Y, X\Y]),
  member(X, [s:F\np, s:F/np]),
  member(Y, [s:G\np, s:G/np]),
  member(F-G, [dcl-b, b-ng, dcl-ng, ng-ng, pt-ng, b-pt, dcl-pt, ng-pt, pt-pt]). % HACK dcl-X could also be modal...

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
