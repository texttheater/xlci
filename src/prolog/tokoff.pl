:- module(tokoff, [
    tokoff_read_file/2]).

:- use_module(library(dialect/sicstus/lists), [
    substitute/4]).
:- use_module(util, [
    line_in_file/2,
    split/4]).

%%	tokoff_read_file(+FileName, -Sentences)
%
%	Reads a PMB-style .tok.off file into a list of lists of =|tokoff/4|=
%	terms. Each list corresponds to a sentence.
tokoff_read_file(FileName, Sentences) :-
  findall(tokoff(From, To, TokID, Token),
      ( line_in_file(Line, FileName),
        split(Line, 32, 3, [FromCodes, ToCodes, TokIDCodes, TokenCodes0]),
        number_codes(From, FromCodes),
        number_codes(To, ToCodes),
        number_codes(TokID, TokIDCodes),
        substitute(32, TokenCodes0, 126, TokenCodes), % replace space with tilde
        atom_codes(Token, TokenCodes)
      ), Rows),
  findall(Bag,
      ( bagof(tokoff(From, To, TokID, Token),
            ( member(tokoff(From, To, TokID, Token), Rows),
              _SentNum is div(TokID, 1000)
            ), Bag)
      ), Sentences).
