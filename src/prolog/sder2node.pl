:- module(der2node, [
    main/0]).

:- use_module(der, [
    sder2node/2,
    pp_der/1]).
:- use_module(node, [
    pp_node/1]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    term_in_file/2]).

main :-
  argv([DerFile]),
  forall(
      ( term_in_file(der(SID, Der), DerFile)
      ),
      ( catch(
            ( sder2node(Der, Node)
            ),
            failed(der:sder2node_(SubDer, _)),
            ( format(user_error, 'ERROR: failed to convert the following sub-derivation~n', []),
              with_output_to(user_error, pp_der(SubDer)),
              halt(1)
            ) ),
        pp_node(node(SID, Node))
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l sder2node -g main DERFILE~n', []),
  halt(1).
