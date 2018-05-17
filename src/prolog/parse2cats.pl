:- module(parse2stagged, [
    main/0]).

:- use_module(cat, [
    strip_var_features/2]).
:- use_module(ccg, [
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
        const_in_ccg(t(Cat, _, _), CCG)
      ),
      ( phrase(cat(Cat), CatCodes),
        format('~s~n', [CatCodes])
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2cats -g main PARSEFILE~n', []),
  halt(1).
