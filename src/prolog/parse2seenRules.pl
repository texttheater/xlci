:- module(parse2seenRules, [
    main/0]).

:- use_module(cat, [
    strip_var_features/2]).
:- use_module(ccg, [
    ccg_cat/2,
    const_in_ccg/2]).
:- use_module(ccgbank, [
    cat//1]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    term_in_file/3]).

main :-
  argv([ParseFile]),
  forall(
      ( term_in_file(ccg(_, CCG), ParseFile, [module(slashes)]),
        const_in_ccg(Const, CCG),
        Const =.. [Rule, _, D1, D2],
        Rule \= lx,
        Rule \= t
      ),
      ( ccg_cat(D1, Cat10),
        ccg_cat(D2, Cat20),
        strip_var_features(Cat10, Cat1),
        strip_var_features(Cat20, Cat2),
        phrase(cat(Cat1), Cat1Codes),
        phrase(cat(Cat2), Cat2Codes),
        format('~s ~s ~w~n', [Cat1Codes, Cat2Codes, Rule])
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2seenRules -g main PARSEFILE | sort -u~n', []),
  halt(1).
