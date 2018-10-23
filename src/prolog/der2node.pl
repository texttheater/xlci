:- module(der2node, [
    main/0]).

:- use_module(der, [
    der2node/2,
    pp_der/1]).
:- use_module(node, [
    pp_node/1]).
:- use_module(slashes).
:- use_module(util, [
    argv/1,
    term_in_file/3]).

main :-
  argv([DerFile]),
  forall(
      ( term_in_file(der(SID, Der), DerFile, [module(slashes)])
      ),
      ( catch(
            ( der2node(Der, Node)
            ),
            failed(der:der2node_(SubDer, _)),
            ( format(user_error, 'ERROR: failed to convert the following sub-derivation~n', []),
              with_output_to(user_error, pp_der(SubDer)),
              halt(1)
            ) ),
        pp_node(node(SID, Node))
      ) ),
  halt.
main :-
  format(user_error, 'USAGE: swipl -l der2node -g main DERFILE~n', []),
  halt(1).
