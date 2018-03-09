:- module(cat, [
    strip_var_features/2]).

strip_var_features(A:F, A) :-
  var(F),
  !.
strip_var_features(C, D) :-
  C =.. [Fun|Args0],
  maplist(strip_var_features, Args0, Args),
  D =.. [Fun|Args].
