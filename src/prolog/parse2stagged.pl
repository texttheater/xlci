:- module(parse2stagged, [
    main/0]).

:- use_module(cat, [
    strip_var_features/2]).
:- use_module(ccg, [
    const_in_ccg/2]).
:- use_module(ccgbank, [
    cat//1]).
:- use_module(library(dialect/sicstus/lists), [
    substitute/4]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    term_in_file/3]).

main :-
  argv([ParseFile]),
  forall(
      ( term_in_file(ccg(_, CCG), ParseFile, [module(slashes)])
      ),
      ( findall(Cat-Form, const_in_ccg(t(Cat, Form, _), CCG), Pairs),
        assertion(Pairs \= []),
        output(Pairs)
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2stagged -g main PARSEFILE~n', []),
  halt(1).

output([Cat0-Form0]) :-
  !,
  strip_var_features(Cat0, Cat),
atom_codes(Form0, FormCodes0),
substitute(32, FormCodes0, 126, FormCodes), % replace space with tilde
atom_codes(Form, FormCodes),
  phrase(cat(Cat), CatCodes),
  format('~w||~s~n', [Form, CatCodes]).
output([Cat0-Form0|Pairs]) :-
  strip_var_features(Cat0, Cat),
atom_codes(Form0, FormCodes0),
substitute(32, FormCodes0, 126, FormCodes), % replace space with tilde
atom_codes(Form, FormCodes),
  phrase(cat(Cat), CatCodes),
  format('~w||~s ', [Form, CatCodes]),
  output(Pairs).
