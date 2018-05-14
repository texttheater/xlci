:- module(derproj, [
    main/0]).

:- use_module(cat, [
  strip_features/2]).
:- use_module(catobj, [
    co2cat/2,
    functor_in/2]).
:- use_module(ccg, [
    node2ccg/2]).
:- use_module(node, [
    node_from_to/3,
    pp_node/1]).
:- use_module(slashes).
:- use_module(sr, [
    parse/2]).
:- use_module(tokoff, [
    tokoff_read_file/2]).
:- use_module(util, [
    argv/1,
    dedup/2,
    line_in_file/2,
    must/1,
    split/4,
    substitute_sub_term/4,
    term_in_file/3,
    write_clause/2,
    write_clause/3]).

:- dynamic source_sentence_catobj/2.
:- dynamic source_catobj/6. % TODO refactor to source_node/4?
:- dynamic source_typechanger/4.
:- dynamic wordalign/4.
:- dynamic target_catobj/6. % TODO refactor to target_node/4?
:- dynamic target_typechanger/4.

main :-
  argv([_, EnglishDerFile, WordAlignFile, ForeignTokOffFile, EnglishTokOffFile, SemanticsFormat, StripFeatures, OutputFormat]), % HACK: first argument is a workaround for https://github.com/SWI-Prolog/swipl-devel/issues/298
  assertion(member(SemanticsFormat, ['boxer', 'offsets'])),
  assertion(member(StripFeatures, ['true', 'false'])),
  assertion(member(OutputFormat, ['parse.tags', 'node'])),
  % Load information from various sources:
  load_source_derivations(EnglishDerFile, StripFeatures),
  %dump_source,
  load_wordalign_file(WordAlignFile),
  %dump_wordalign,
  tokoff_read_file(ForeignTokOffFile, ForeignSentences),
  tokoff_read_file(EnglishTokOffFile, EnglishSentences),
  % Project:
  transfer_categories(ForeignSentences, EnglishSentences, SemanticsFormat),
  transfer_typechangers,
  flip_slashes,
  %dump_target,
  create_derivations(ForeignSentences, OutputFormat),
  halt.
main :-
  format(user_error, 'USAGE (example): swipl -l derproj -g main en.node nl.wordalign nl.tok.off en.tok.off boxer false parse.tags~n', []),
  halt(1).

%%% CORE PROJECTION PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asserts target_catobj/5 facts mapping target tokens to category objects.
transfer_categories(ForeignSentences, EnglishSentences, SemanticsFormat) :-
  forall(
      ( nth1(SID, ForeignSentences, ForeignSentence),
        member(tokoff(ForFrom, ForTo, _TokID, Token), ForeignSentence)
      ),
      ( forall(
	    ( wordalign(SID, ForFrom, ForTo, EnglishOffsets)
	    ),
	    ( findall(EngFrom-EngTo,
                ( wordalign(SID, ForFrom, ForTo, EnglishOffsets),
                  member(EngFrom-EngTo, EnglishOffsets)
                ), Pairs),
              (  Pairs = [] % nothing aligned
              -> true % TODO warn when nothing is aligned to a token in *any* alignment?
              ;  Pairs = [_, _|_] % more than one token aligned, need to combine them
              -> findall(EngTokoff,
                     ( member(EngFrom-EngTo, Pairs),
                       once(
                           ( nth1(SID, EnglishSentences, EnglishSentence),
                             member(EngTokoff, EnglishSentence),
                             EngTokoff = tokoff(EngFrom, EngTo, _, _)
                           ) )
                     ), EngPhrase),
                 make_item(source_catobj, source_typechanger, SID, EngPhrase, Item),
                 parse([Item], Parses),
                 (  Parses = []
                 -> format(user_error, 'no parse for phrase aligned to token "~w" (~w, ~w, ~w), skipping~n', [Token, SID, ForFrom, ForTo])
                 ;  Parses = [_, _|_]
                 -> format(user_error, 'WARNING: ambiguous phrase aligned to token "~w" (~w, ~w, ~w), skipping~n', [Token, SID, ForFrom, ForTo])
                 ;  Parses = [item([node(CO, Sem0, _, _)], [], _, true)]
                 -> replace_indices(ForFrom, ForTo, Sem0, Sem, SemanticsFormat),
                    assertz(target_catobj(SID, ForFrom, ForTo, CO, Sem, [from:ForFrom, to:ForTo]))
                 )
              ;  Pairs = [EngFrom-EngTo], % one token aligned, easy
                 source_catobj(SID, EngFrom, EngTo, CO, Sem0, Atts)
              -> replace_indices(ForFrom, ForTo, Sem0, Sem, SemanticsFormat),
                 assertz(target_catobj(SID, ForFrom, ForTo, CO, Sem, [from:ForFrom, to:ForTo|Atts]))
              ;  format(user_error, 'token "~w" (~w, ~w, ~w) one-to-one aligned but no category object found, skipping~n', [Token, SID, ForFrom, ForTo])
              )
	    ) ) ) ).

% Asserts target_typechanger/4 facts mapping target tokens to typechangers.
transfer_typechangers :-
  findall(target_typechanger(SID, ForFrom, ForTo, TC),
      ( source_typechanger(SID, TCFrom, TCTo, TC),
	wordalign(SID, ForFrom, ForTo, EngOffsets),
	target_catobj(SID, ForFrom, ForTo, _, _, _),
	member(EngFrom-EngTo, EngOffsets),
	EngFrom >= TCFrom,
	EngTo =< TCTo
      ), TCs),
  sort(TCs, TCSet),
  maplist(assertz, TCSet).

flip_slashes :-
  flip_slashes_functors,
  flip_slashes_args.

% Flip slashes, phase 1:
% Make the slashes of all functors lean towards their arguments.
% Also change co(ID, Cat, UCat) terms that appear as argument categories
% of modifiers to match the slash directions of the result categories.
flip_slashes_functors :-
  forall(
      ( retract(target_catobj(SID, From, To, CO, Sem, Atts))
      ),
      ( findall(FlipCO, flip_slashes_functors(CO, SID, From, To, FlipCO), FlipCOs0),
        dedup(FlipCOs0, FlipCOs),
	forall(
	    ( member(FlipCO, FlipCOs)
	    ),
	    ( assertz(target_catobj(SID, From, To, FlipCO, Sem, Atts))
	    ) )
      ) ),
  forall(
      ( retract(target_typechanger(SID, From, To, tc(NewCO-OldCO, Sem)))
      ),
      ( findall(FlipCO, flip_slashes_functors(NewCO, SID, From, To, FlipCO), FlipCOs0),
	dedup(FlipCOs0, FlipCOs),
	forall(
	    ( member(FlipCO, FlipCOs)
	    ),
	    ( assertz(target_typechanger(SID, From, To, tc(FlipCO-OldCO, Sem)))
	    ) )
      ) ).

flip_slashes_functors(X/Y, SID, From, To, FlipCO) :-
  flip_slashes_functors(X, Y, SID, From, To, FlipCO).
flip_slashes_functors(X\Y, SID, From, To, FlipCO) :-
  flip_slashes_functors(X, Y, SID, From, To, FlipCO).
flip_slashes_functors(co(ID, Cat, UCat), _, _, _, co(ID, Cat, UCat)).

flip_slashes_functors(X, Y, SID, XFrom, XTo, FlipX/FlipY) :-
  functor_from_to(Y, SID, YFrom, _),
  YFrom > XTo, % X before Y -> forward slash
  flip_slashes_functors(X, SID, XFrom, XTo, FlipX),
  flip_slashes_functor_arg(X, Y, FlipX, FlipY).
flip_slashes_functors(X, Y, SID, XFrom, XTo, FlipX\FlipY) :-
  functor_from_to(Y, SID, _, YTo),
  YTo < XFrom, % X after Y -> backward slash
  flip_slashes_functors(X, SID, XFrom, XTo, FlipX),
  flip_slashes_functor_arg(X, Y, FlipX, FlipY).

% Adapt the slash directions of arguments of modifiers
flip_slashes_functor_arg(X, co(ID, YCat, YUCat), FlipX, co(ID, YFlipCat, YUFlipCat)) :-
  co2cat(X, YCat), % test if it's a modifier
  !,
  flip_slashes_functor_arg_cat(FlipX, YCat, YUCat, YFlipCat, YUFlipCat).
flip_slashes_functor_arg(_, Y, _, Y).

flip_slashes_functor_arg_cat(co(_, _, _), Cat, UCat, Cat, UCat).
flip_slashes_functor_arg_cat(A/B, C/D, E/F, FlipC/FlipD, FlipE/FlipF) :-
  flip_slashes_functor_arg_cat(A, C, E, FlipC, FlipE),
  flip_slashes_functor_arg_cat(B, D, F, FlipD, FlipF).
flip_slashes_functor_arg_cat(A/B, C\D, E\F, FlipC/FlipD, FlipE/FlipF) :-
  flip_slashes_functor_arg_cat(A, C, E, FlipC, FlipE),
  flip_slashes_functor_arg_cat(B, D, F, FlipD, FlipF).
flip_slashes_functor_arg_cat(A\B, C/D, E/F, FlipC\FlipD, FlipE\FlipF) :-
  flip_slashes_functor_arg_cat(A, C, E, FlipC, FlipE),
  flip_slashes_functor_arg_cat(B, D, F, FlipD, FlipF).
flip_slashes_functor_arg_cat(A\B, C\D, E\F, FlipC\FlipD, FlipE\FlipF) :-
  flip_slashes_functor_arg_cat(A, C, E, FlipC, FlipE),
  flip_slashes_functor_arg_cat(B, D, F, FlipD, FlipF).

flip_slashes_args :-
  forall(
      ( retract(target_catobj(SID, From, To, CO, Sem, Atts))
      ),
      ( findall(FlipCO, flip_slashes_args(SID, CO, FlipCO), FlipCOs0),
	dedup(FlipCOs0, FlipCOs),
	forall(
	    ( member(FlipCO, FlipCOs)
	    ),
	    ( assertz(target_catobj(SID, From, To, FlipCO, Sem, Atts))
	    ) )
      ) ),
  forall(
      ( retract(target_typechanger(SID, From, To, tc(NewCO-OldCO, Sem)))
      ),
      ( findall(FlipCO, flip_slashes_args(SID, NewCO, FlipCO), FlipCOs0),
	dedup(FlipCOs0, FlipCOs),
	forall(
	    ( member(FlipCO, FlipCOs)
	    ),
	    ( assertz(target_typechanger(SID, From, To, tc(FlipCO-OldCO, Sem)))
	    ) )
      ) ).

flip_slashes_args(SID, co(ID, _, _), co(ID, Cat, UCat)) :-
  functor_from_to(_/co(ID, Cat, UCat), SID, _, _),
  !.
flip_slashes_args(SID, co(ID, _, _), co(ID, Cat, UCat)) :-
  functor_from_to(_\co(ID, Cat, UCat), SID, _, _),
  !.
flip_slashes_args(_, co(ID, Cat, UCat), co(ID, Cat, UCat)).
flip_slashes_args(SID, X/Y, FlipX/Y) :-
  flip_slashes_args(SID, X, FlipX).
flip_slashes_args(SID, X\Y, FlipX\Y) :-
  flip_slashes_args(SID, X, FlipX).

create_derivations(ForeignSentences, Format) :-
  forall(
      ( nth1(SID, ForeignSentences, ForeignSentence)
      ),
      ( % Determine target category object:
        (  source_sentence_catobj(SID, TargetCO)
        -> true
        ;  format(user_error, 'WARNING: no target category object for sentence ~w found (did not find or failed to analyze 1:1 aligned source sentence)~n', [SID]),
           TargetCO = dummy
        ),
        % Parse the foreign sentence using projected categories:
        make_item(target_catobj, target_typechanger, SID, ForeignSentence, Item),
        catch(
            ( parse([Item], Agenda0)
            ), agenda_limit_exceeded,
            ( format(user_error, 'WARNING: parser agenda limit exceeded for sentence ~w~n', [SID]),
              Agenda0 = []
            ) ),
        include(hits_target(TargetCO), Agenda0, Agenda),
        length(Agenda, Length),
        (  Length = 0
        -> format(user_error, 'WARNING: no parse projected for sentence ~w~n', [SID])
        ;  (  Length > 1
           -> format(user_error, 'WARNING: ~w different parses projected for sentence ~w~n', [Length, SID])
           ;  true
           ),
	   format(user_error, 'INFO: parse projected for sentence ~w~n', [SID]),
           Agenda = [item([Node], [], _, true)|_],
	   %with_output_to(user_error, pp_node(Node)),
	   (  Format == node
	   -> pp_node(node(SID, Node))
	   ;  node2ccg(Node, CCG),
              write_clause(current_output, ccg(SID, CCG), [module(slashes)])
	   )
        )
      ) ).

%%% LOADERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Asserts source_catobj/6 and source_typechanger/4 facts mapping source offset
% pairs to lexical category objects and to typechangers.
load_source_derivations(EnglishNodeFile, StripFeatures) :-
  forall(
      ( term_in_file(node(SID, Node), EnglishNodeFile, [module(slashes)])
      ),
      ( % Get lexical category objects:
        forall(
            ( sub_node(node(CO0, Sem, t(_Form, Atts0), []), Node)
            ),
            ( select(from:From, Atts0, Atts1),
              select(to:To, Atts1, Atts),
	      (  StripFeatures
	      -> strip_features(CO0, CO)
	      ;  CO0 = CO
	      ),
              assertz(source_catobj(SID, From, To, CO, Sem, Atts))
            ) ),
        % Get type changers:
        forall(
            ( sub_node(node(X0, _, tc(TCSem), [Child]), Node)
            ),
            ( Child = node(Y0, _, _, _),
              node_from_to(Child, From, To),
	      ( StripFeatures
	      -> strip_features(X0, X),
		 strip_features(Y0, Y)
	      ;  X0 = X,
		 Y0 = Y
	      ),
              assertz(source_typechanger(SID, From, To, tc(X-Y, TCSem)))
            ) ),
        % Get sentence category object:
        Node = node(CO0, _, _, _),
	(  StripFeatures
	-> strip_features(CO0, CO)
	;  CO0 = CO
	),
        assertz(source_sentence_catobj(SID, CO))
      ) ).

% Reads the word alignment file and asserts its contents as wordalign/4 facts.
load_wordalign_file(WordAlignFile) :-
  findall(Line, line_in_file(Line, WordAlignFile), Lines),
  split(Lines, [], infinity, Blocks),
  forall(
      ( nth1(SID, Blocks, Block)
      ),
      ( findall(wordalign(SID, ForeignFrom, ForeignTo, EnglishOffsetsList),
	    ( member(Line, Block),
	      line_wordalign(Line, ForeignFrom, ForeignTo, EnglishOffsetsList)
	    ), Clauses0),
	dedup(Clauses0, Clauses),
	maplist(assertz, Clauses)
      ) ).

line_wordalign(Line, ForeignFrom, ForeignTo, EnglishOffsetsList) :-
  must(split(Line, 9, infinity, [ForeignFromCodes, ForeignToCodes, EnglishOffsetsCodes])),
  number_codes(ForeignFrom, ForeignFromCodes),
  number_codes(ForeignTo, ForeignToCodes),
  must(split(EnglishOffsetsCodes, 32, infinity, PairCodess)),
  findall(EnglishFrom-EnglishTo,
      ( member(PairCodes, PairCodess),
        must(split(PairCodes, 44, infinity, [EnglishFromCodes, EnglishToCodes])),
        number_codes(EnglishFrom, EnglishFromCodes),
        number_codes(EnglishTo, EnglishToCodes)
      ), EnglishOffsetsList).

%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hits_target(TargetCO, item([node(TargetCO, _, _, _)], _, _, _)).

functor_from_to(Functor, SID, From, To) :-
  target_catobj(SID, From, To, CO, _, _),
  functor_in(Functor, CO).
functor_from_to(Functor, SID, From, To) :-
  target_typechanger(SID, From, To, tc(CO, _)),
  functor_in(Functor, CO).

% With target_catobj/6 (source_catobj/6) and target_typechanger/4
% (source_typechanger/4) facts asserted, creates the initial shift-reduce
% parser item for the target sentence.
make_item(_, _, _, [], item([], [], [], false)).
make_item(COPred, TCPred, SID, [tokoff(From, To, _TokID, Token)|Sent], item([], [Choices|Queue], TypeChangers, false)) :-
  % Recursively generate the initial item for the rest of the sentence:
  make_item(COPred, TCPred, SID, Sent, item([], Queue, TypeChangers0, false)),
  findall(node(CO, Sem, t(Token, Atts), []), call(COPred, SID, From, To, CO, Sem, Atts), Choices),
  findall(TC,
      ( call(TCPred, SID, From, To, TC),
        \+ member(TC, TypeChangers0)
      ), NewTypeChangers),
  append(NewTypeChangers, TypeChangers0, TypeChangers).

sub_node(Node, Node).
sub_node(Sub, node(_, _, _, Children)) :-
  member(Child, Children),
  sub_node(Sub, Child).

sentence_from_to(Sentence, From, To) :-
  Sentence = [tokoff(From, _, _, _)|_],
  last(Sentence, tokoff(_, To, _, _)).

replace_indices(ForFrom, ForTo, Sem0, Sem, boxer) :-
  substitute_sub_term(P:[_|_]:C, P:[ForFrom, ForTo]:C, Sem0, Sem).
replace_indices(ForFrom, ForTo, _Sem0, ForFrom-ForTo, offsets).

dump_wordalign :-
  forall(
      ( wordalign(SID, ForeignFrom, ForeignTo, EnglishOffsetsList)
      ),
      ( write_clause(user_error, wordalign(SID, ForeignFrom, ForeignTo, EnglishOffsetsList))
      ) ).

dump_source :-
  forall(
      ( source_catobj(A, B, C, D, E, F)
      ),
      ( write_clause(user_error, source_catobj(A, B, C, D, E, F), [module(slashes)])
      ) ),
  forall(
      ( source_typechanger(A, B, C, D)
      ),
      ( write_clause(user_error, source_typechanger(A, B, C, D), [module(slashes)])
      ) ).

dump_target :-
  forall(
      ( target_catobj(A, B, C, D, E, F)
      ),
      ( write_clause(user_error, target_catobj(A, B, C, D, E, F), [module(slashes)])
      ) ),
  forall(
      ( target_typechanger(A, B, C, D)
      ),
      ( write_clause(user_error, target_typechanger(A, B, C, D), [module(slashes)])
      ) ).
