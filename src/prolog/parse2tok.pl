:- module(parse2raw, [
    main/0]).

:- use_module(ccg, [
    const_in_ccg/2]).
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
      ( findall(Form, const_in_ccg(t(_, Form, _), CCG), Forms),
        assertion(Forms \= []),
        output(Forms)
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l parse2tok -g main PARSEFILE~n', []),
  halt(1).

output([Form0]) :-
  !,
atom_codes(Form0, FormCodes0),
substitute(32, FormCodes0, 126, FormCodes), % replace space with tilde
atom_codes(Form, FormCodes),
  write(Form),
  nl.
output([Form0|Forms]) :-
atom_codes(Form0, FormCodes0),
substitute(32, FormCodes0, 126, FormCodes), % replace space with tilde
atom_codes(Form, FormCodes),
  write(Form),
  write(' '),
  output(Forms).
