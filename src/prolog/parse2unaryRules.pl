:- module(parse2unaryRules, [
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
        (  Const = lx(NewCat0, OldCat0, _)
        ;  Const = tr(NewCat0, D),
           ccg_cat(D, OldCat0)
        )
      ),
      ( strip_var_features(NewCat0, NewCat),
        strip_var_features(OldCat0, OldCat),
        phrase(cat(NewCat), NewCatCodes),
        phrase(cat(OldCat), OldCatCodes),
        format('~s\t~s~n', [OldCatCodes, NewCatCodes])
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2unaryRules -g main PARSEFILE | sort -u~n', []),
  halt(1).
