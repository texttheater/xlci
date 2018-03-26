:- module(der2node, [
    main/0]).

:- use_module(der, [
    der2node/2]).
:- use_module(node, [
    pp_node/1]).
:- use_module(util, [
    argv/1,
    term_in_file/2]).

main :-
  argv([DerFile]),
  forall(
      ( term_in_file(der(SID, Der), DerFile)
      ),
      ( der2node(Der, Node),
        pp_node(node(SID, Node))
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l der2node -g main DERFILE~n', []),
  halt(1).
