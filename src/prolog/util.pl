:- module(util, [
    argv/1,
    atom_upper/2,
    codes//2,
    enumerate/2,
    enumerate/3,
    funsort/3,
    line_in_file/2,
    line_in_stream/2,
    log/1,
    log/2,
    maplist/6,
    must/1,
    print_indented/2,
    print_indented/3,
    rsplit/4,
    split/4,
    substitute_sub_term/3,
    substitute_sub_term/4,
    subsumed_sub_term/2,
    list_occurrences_of_term/3,
    term_in_file/2,
    term_in_file/3,
    term_in_stream/2,
    term_in_stream/3,
    times/2,
    write_atom_quoted/1,
    write_clause/1,
    write_clause/2,
    write_clause/3,
    write_term_vars/1,
    write_term_vars/2,
    write_term_vars/3]).

%%	argv(-List) is det.
%
%	True iff List is the list of arguments the program was called with,
%	excluding those processed by the SWI-Prolog runtime.
argv(Argv) :-
  (  current_prolog_flag(os_argv, _)
  -> current_prolog_flag(argv, Argv)
  ;  throw('SWI-Prolog 6.5+ required')
  ).

%%	term_in_file(-Codes, +FileName) is nondet.
%
%	Opens the specified Prolog file and succeeds once for every term in it.
term_in_file(Term, File) :-
  term_in_file(Term, File, []).

%%	term_in_file(-Codes, +FileName) is nondet.
%
%	Opens the specified Prolog file and succeeds once for every term in it.
%	Options is passed to =|read_term/3|=.
term_in_file(Term, File, Options) :-
  setup_call_cleanup(
      ( open(File, read, Stream)
      ),
      ( term_in_stream(Term, Stream, Options)
      ),
      ( close(Stream)
      ) ).

%%	term_in_stream(-Codes, +FileName) is nondet.
%
%	Reads Prolog terms from Stream and succeeds once for every term read.
term_in_stream(Term, Stream) :-
  term_in_stream(Term, Stream, []).

%%	term_in_stream(-Codes, +FileName) is nondet.
%
%	Reads Prolog terms from Stream and succeeds once for every term read.
%	Options is passed to =|read_term/3|=.
term_in_stream(Term, Stream, Options) :-
  repeat,
  read_term(Stream, ReadTerm, Options),
  (  ReadTerm == end_of_file
  -> !,
     fail
  ;  Term = ReadTerm
  ).

%%	line_in_file(-Codes, +FileName) is nondet.
%
%	Opens the specified file and succeeds once for every line in it.
line_in_file(Codes, File) :-
  setup_call_cleanup(
      ( open(File, read, Stream)
      ),
      ( line_in_stream(Codes, Stream)
      ),
      ( close(Stream)
      ) ).

%%	line_in_stream(-Codes, +Stream) is nondet.
%
%	Reads from Stream and succeeds once for every line read from it.
line_in_stream(Codes, Stream) :-
  repeat,
  read_line_to_codes(Stream, Codes),
  (  Codes == end_of_file
  -> !,
     fail
  ;  true
  ).

%%	lines_in_file(-Lines, +FileName)
%
%	Reads all lines in the specified file into a list of code lists.
lines_in_file(Lines, FileName) :-
  findall(Line, line_in_file(Line, FileName), Lines).

%%	substitute_sub_term(+OldSubTerm, +NewSubTerm, +OldTerm, -NewTerm) is det
%
%	NewTerm is OldTerm, but with all occurrences subsumed by OldSubTerm replaced
%	by a corresponding instantiation of NewSubTerm.
substitute_sub_term(OldSubTerm, NewSubTerm, OldTerm, NewTerm) :-
  substitute_sub_term(sst(copy_term, OldSubTerm, NewSubTerm), OldTerm, NewTerm).

sst(Eq, OldSubTerm, NewSubTerm, OldTerm, NewTerm) :-
  subsumes_term(OldSubTerm, OldTerm),
  call(Eq, OldSubTerm-NewSubTerm, OldTerm-NewTerm).

:- meta_predicate substitute_sub_term(2, +, -).

%%	substitute_sub_term(+Function, +OldTerm, -NewTerm) is det
%	NewTerm is OldTerm, but with every subterm OldSubTerm replaced by NewSubTerm
%	if =|call(Function, OldSubTerm, NewSubTerm)|= succeeds.
substitute_sub_term(Function, OldTerm, NewTerm) :-
  call(Function, OldTerm, NewTerm),
  !.
substitute_sub_term(Function, OldTerm, NewTerm) :-
  compound(OldTerm),
  !,
  OldTerm =.. [Name|OldArgs],
  maplist(substitute_sub_term(Function), OldArgs, NewArgs),
  NewTerm =.. [Name|NewArgs].
substitute_sub_term(_, Term, Term).

%%	split(+List, +Sep, +Max, -Lists)
%
%	Splits List at element Sep into Lists. At most Max splits are made. Max
%	can be infinity.
split([], _Sep, _Max, []) :- % empty list -> no blocks
  !.
split(List, _Sep, 0, [List]) :- % no more splits -> rest is one block
  !.
split(List, Sep, Max, [Block|Blocks]) :-
  list_sep_block_rest(List, Sep, Block, Rest), % a split is made
  !,
  dec(Max, NewMax),
  (  Rest = []
  -> Blocks = [[]] % separator was at the end -> last block is empty
  ;  split(Rest, Sep, NewMax, Blocks) % separator was not at the end -> recursion
  ).
split(List, _Sep, _Max, [List]). % no separator left -> rest is one block

list_sep_block_rest([Sep|Rest], Sep, [], Rest) :-
  !.
list_sep_block_rest([First|Rest0], Sep, [First|Block], Rest) :-
  list_sep_block_rest(Rest0, Sep, Block, Rest).

%%	rsplit(+List, +Sep, +Max, -Lists)
%
%	Splits List at element Sep into Lists. At most Max splits are made from the
%	right. Max can be infinity.
rsplit(List, Sep, Max, Lists) :-
  reverse(List, Tsil),
  split(Tsil, Sep, Max, Stsil),
  reverse(Stsil, Lists0),
  maplist(reverse, Lists0, Lists).

dec(infinity, infinity) :-
  !.
dec(Int, Dec) :-
  Dec is Int - 1.

%%	write_clause(+Term)
%%	write_clause(+Stream, +Term) 
%%	write_clause(+Stream, +Term, +Options)
%
%	Utility for writing Prolog database clauses to files etc. Writes the given
%	Term to Stream (=current_output= for the 1-arg version), followed by a
%	period and a newline. The Options are passed to =|write_term_vars/3|=.
write_clause(Term) :-
  write_clause(current_output, Term).

write_clause(Stream, Term) :-
  write_clause(Stream, Term, []).

write_clause(Stream, Term, Options) :-
  write_term_vars(Stream, Term, [quoted(true), fullstop(true), nl(true)|Options]).

%%	write_term_vars(+Term)
%%	write_term_vars(+Term, +Options)
%%	write_term_vars(+Stream, +Term, +Options)
%
%	Like =|write_term/[1,2,3]|=, but automatically assigns variable names.

write_term_vars(Term) :-
  write_term_vars(Term, []).

write_term_vars(Term, Options) :-
  write_term_vars(current_output, Term, Options).

write_term_vars(Stream, Term, Options) :-
  \+ \+ ( numbervars(Term, 0, _, [singletons(true)]),
          write_term(Stream, Term, [numbervars(true)|Options])
        ).

%%	subsumed_sub_term(?Template,+Term)
%
%	Like sub_term/2, but without those solutions that would make Term
%	more specific. In other words, succeeds once for every subterm of
%	Term that is subsumed by Template.
subsumed_sub_term(Template,Term) :-
  sub_term(Sub, Term),
  subsumes_term(Template, Sub),
  Template = Sub.

:- meta_predicate enumerate(-, 0).
:- meta_predicate enumerate(-, 0, +).

%%	enumerate(-I, +Goal)
%%	enumerate(-I, +Goal, +Offset).
%
%	Like Goal, but on each solution, I is bound to the number of the solution,
%	starting with Offset + 1, or if Offset is not given, with 1.
enumerate(I, Goal) :-
  enumerate(I, Goal, 0).

enumerate(I, Goal, Offset) :-
  State = state(Offset),
  call(Goal),
  arg(1, State, I0),
  I is I0 + 1,
  nb_setarg(1, State, I).

%%	print_indented(+Term, +StopPatterns)
%%	print_indented(+Term, +StopPatterns, +Options)
%
%	Pretty-prints Term on multiple lines, indenting it according to its
%	tree structure. StopPatterns is a list of terms. Any subterm of Term
%	unifying with one of them will be printed normally, without newlines or
%	indentation.
%
%	The following Options are interpreted:
%
%	* =|fullstop(Bool)|=
%	  If =true= (default =false=), add a fullstop token to the output.
%	
%	Other options are passed to =|write_term/3|=.
print_indented(Term, StopPatterns) :-
  print_indented(Term, StopPatterns, []).

print_indented(Term, StopPatterns, Options) :-
  (  select(fullstop(FullStop), Options, WriteTermOptions)
  -> true
  ;  Options = WriteTermOptions,
     FullStop = false
  ),
  \+ \+ ( numbervars(Term, 0, _, [singletons(true)]),
          pi(0, Term, StopPatterns, WriteTermOptions)
        ),
  (  FullStop == true
  -> write('.')
  ;  true
  ),
  nl.

pi(Level, Term, StopPatterns, Options) :-
  times(write(' '), Level),
  pi_term(Level, Term, StopPatterns, Options).

pi_term(Level, Term, StopPatterns, Options) :-
  (  ( var(Term)
     ; Term = '$VAR'(_)
     ; Term =.. [_]
     ; pi_stop(Term, StopPatterns)
     )
  -> write_term(current_output, Term, [numbervars(true), quoted(true)|Options])
  ;  Term = [_|_]
  -> pi_list(Level, Term, StopPatterns, Options)
  ;  Term = Label:InnerTerm
  -> write_term(current_output, Label, [numbervars(true), quoted(true)|Options]),
     write(':'),
     pi_term(Level, InnerTerm, StopPatterns, Options)
  ;  Term =.. [Name|Args],
     write_canonical(Name),
     write('('),
     nl,
     NewLevel is Level + 1,
     pi_args(NewLevel, Args, StopPatterns, Options)
  ).

pi_stop(Term, StopPatterns) :-
  member(StopPattern, StopPatterns),
  subsumes_term(StopPattern, Term),
  !.

% TODO handle open-ended lists correctly
pi_list(Level, List, StopPatterns, Options):-
  write('['),
  nl,
  NewLevel is Level + 1,
  pi_members(NewLevel, List, StopPatterns, Options),
  times(write(' '), Level),
  write(']').

pi_members(Level, [Member], StopPatterns, Options) :-
  !,
  pi(Level, Member, StopPatterns, Options),
  nl.
pi_members(Level, [Member|Members], StopPatterns, Options) :-
  pi(Level, Member, StopPatterns, Options),
  write(','),
  nl,
  pi_members(Level, Members, StopPatterns, Options).

pi_args(Level, [Arg], StopPatterns, Options) :-
  !,
  pi(Level, Arg, StopPatterns, Options),
  write(')').
pi_args(Level, [Arg|Args], StopPatterns, Options) :-
  pi(Level, Arg, StopPatterns, Options),
  write(','),
  nl,
  pi_args(Level, Args, StopPatterns, Options).

%%	times(+Goal, +N)
%
%	Call a goal a fixed number of times.
times(_, 0) :-
  !.
times(Goal, Times) :-
  Goal,
  NewTimes is Times - 1,
  times(Goal, NewTimes).

:- meta_predicate funsort(2, +, -).

%%	funsort(:Function, +List, -Sorted)
%
%	Sort similar to =|sort/2|=, but determines the order of two terms by
%	applying Function to them and sorting by the result.
%
%	This is a convenience wrapper around =|map_list_to_pairs/3|=,
%	=|keysort/2|= and =|pairs_values/2|=. See also =|predsort/3|=.
funsort(Function, List, Sorted) :-
  map_list_to_pairs(Function, List, Keyed),
  keysort(Keyed, KeyedSorted),
  pairs_values(KeyedSorted, Sorted).

%%	list_occurrences_of_term(+SubTerm, +Term, -Occurrences)
%
%	Like occurrences_of_term/3, but gives you the list of occurrences
%	rather than their count.
list_occurrences_of_term(SubTerm, Term, Occurrences) :-
  list_occurrences_of_term(SubTerm, Term, Occurrences, []).

list_occurrences_of_term(SubTerm, Term, [Term|Occurrences], Occurrences) :-
  var(Term),
  subsumes_term(SubTerm, Term),
  !.
list_occurrences_of_term(_, Term, Occurrences, Occurrences) :-
  var(Term),
  !.
list_occurrences_of_term(SubTerm, Term, [Term|Occurrences0], Occurrences) :-
  subsumes_term(SubTerm, Term),
  !,
  Term =.. [_|Args],
  list_occurrences_of_term_list(SubTerm, Args, Occurrences0, Occurrences).
list_occurrences_of_term(SubTerm, Term, Occurrences0, Occurrences) :-
  Term =.. [_|Args],
  list_occurrences_of_term_list(SubTerm, Args, Occurrences0, Occurrences).

list_occurrences_of_term_list(_, [], Occurrences, Occurrences) :-
  !.
list_occurrences_of_term_list(SubTerm, [First|Rest], Occurrences0, Occurrences) :-
  list_occurrences_of_term(SubTerm, First, Occurrences0, Occurrences1),
  list_occurrences_of_term_list(SubTerm, Rest, Occurrences1, Occurrences).

code(Type, Code) -->
  [Code],
  { code_type(Code, Type)
  }.

codes(Type, [H|T]) -->
  code(Type, H),
  codes(Type, T).
codes(_, []) -->
  [].

write_atom_quoted(Atom) :-
  with_output_to(codes(Codes), write_term(Atom, [quoted(true)])),
  (  Codes = [39|_] % starts with ' -> already quoted
  -> format('~s', [Codes])
  ;  format('\'~s\'', [Codes])
  ).

atom_upper(Atom, ATOM) :-
  atom_string(Atom, String),
  string_upper(String, STRING),
  atom_string(ATOM, STRING).

:- meta_predicate must(0).

must(Goal) :-
  once(Goal),
  !.
must(Goal) :-
  throw(failed(Goal)).

:- meta_predicate maplist(5, ?, ?, ?, ?, ?).

%!  maplist(:Goal, ?List1, ?List2, ?List3, ?List4, ?List5)
%
%   As maplist/2, operating on  quintuples   of  elements  from five
%   lists.

maplist(Goal, List1, List2, List3, List4, List5) :-
    maplist_(List1, List2, List3, List4, List5, Goal).

maplist_([], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], Goal) :-
    call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5),
    maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Goal).

%%	log(+Format, +Args)
%%	log(+Term)
%
%	Quick and dirty logging facility.

log(Format, Args) :-
  format(user_error, Format, Args).

log(Term) :-
  write_term_vars(user_error, Term, [quoted(true), nl(true)]).
