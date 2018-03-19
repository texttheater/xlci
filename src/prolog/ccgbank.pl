:- module(ccgbank,
    [cat//1]).

:- use_module(slashes).
:- use_module(util, [
    codes//2]).

:- set_prolog_flag(double_quotes, codes).

cat(conj:Cat) -->
  complex(Cat),
  feat(conj),
  !.
cat(conj:Cat) -->
  basic(Cat),
  feat(conj).
cat(Cat) -->
  complex(Cat).
cat(Cat) -->
  basic(Cat).

cat_emb(Cat) -->
  "(",
  complex(Cat),
  ")".
cat_emb(conj:Cat) -->
  "(",
  complex(Cat),
  feat(conj),
  ")".
cat_emb(conj:Cat) -->
  basic(Cat),
  feat(conj).
cat_emb(Cat) -->
  basic(Cat).

complex(Fun/Arg) -->
  cat_emb(Fun),
  "/",
  cat_emb(Arg).
complex(Fun\Arg) -->
  cat_emb(Fun),
  "\\",
  cat_emb(Arg).

basic(Atomic:Feat) -->
  atomic(Atomic),
  feat(Feat),
  { Feat \== conj
  }.
basic(Atomic) -->
  atomic(Atomic).

feat(Feat) -->
  { var(Feat)
  },
  !,
  "[",
  codes(lower, FeatCodes),
  { atom_codes(Feat, FeatCodes)
  },
  "]".
feat(Feat) -->
  { atom(Feat)
  },
  !,
  "[",
  { atom_codes(Feat, FeatCodes)
  },
  codes(lower, FeatCodes),
  "]".

% TODO clean this up
atomic(comma) -->
  ",",
  !.
atomic(',') -->
  ",",
  !.
atomic(period) -->
  ".",
  !.
atomic('.') -->
  ".",
  !.
atomic(semi) -->
  ";",
  !.
atomic(colon) -->
  ":",
  !.
atomic(conj) -->
  "conj",
  !.
atomic(rrb) -->
  "RRB",
  !.
atomic(lrb) -->
  "LRB",
  !.
atomic(Atomic) -->
  { var(Atomic)
  },
  !,
  codes(upper, AtomicCodes),
  { atom_codes(AtomicAtom, AtomicCodes),
    downcase_atom(AtomicAtom, Atomic)
  }.
atomic(Atomic) -->
  { atom(Atomic),
    Atomic \= app
  },
  !,
  { upcase_atom(Atomic, AtomicAtom),
    atom_codes(AtomicAtom, AtomicCodes)
  },
  codes(upper, AtomicCodes).

:- begin_tests(ccgbank).

:- set_prolog_flag(double_quotes, codes).

test(cat_parse) :-
  findall(Cat, phrase(cat(Cat), "(S[dcl]\\NP)/NP"), Cats),
  Cats == [((s:dcl)\np)/np].

test(cat_parse2) :-
  findall(Cat, phrase(cat(Cat), "conj[app]"), Cats),
  Cats == [conj:app].

test(cat_parse2) :-
  findall(Cat, phrase(cat(Cat), "S/NP[conj]"), Cats),
  Cats == [conj:(s/np)].

test(cat_generate) :-
  findall(String, phrase(cat(((s:dcl)\np)/np), String), Strings),
  Strings == ["(S[dcl]\\NP)/NP"].

test(cat_generate2) :-
  findall(String, phrase(cat(conj:app), String), Strings),
  Strings == ["conj[app]"].

:- end_tests(ccgbank).
