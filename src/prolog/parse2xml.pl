:- module(parse2xml, [
    main/0]).

:- use_module(slashes).
:- use_module(util, [
    argv/1,
    atom_upper/2,
    term_in_file/3]).

main :-
  argv([ParseFile]),
  format('<?xml version="1.0" encoding="UTF-8"?>~n'),
  format('<!DOCTYPE xdrs-output SYSTEM "src/data/boxer/xdrs.dtd">~n'),
  format('<xdrs-output>~n'),
  forall(
      ( term_in_file(ccg(Num, Const), ParseFile, [module(slashes)])
      ),
      ( format(' <der id="~w">~n', [Num]),
	const2xml(Const),
	format(' </der>~n')
      ) ),
  format('</xdrs-output>~n'),
  halt(0).
main :-
  format(user_error, 'USAGE: swipl -l parse2xml.pl -g main PARSEFILE > XMLFILE~n', []),
  halt(1).

const2xml(ba(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="ba" description="Backward Application">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(t(Cat, Token, Atts)) :-
  !,
  member(from:From, Atts),
  member(to:To, Atts),
  !,
  format('<lex>'),
  xml_quote_cdata(Token, TokenQ, utf8),
  format('<token>~w</token>', [TokenQ]),
  format('<tag type="from">~w</tag>', [From]),
  format('<tag type="to">~w</tag>', [To]),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  format('</lex>').
const2xml(fa(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="fa" description="Forward Application">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(bc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="bc" description="Backward Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(fc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="fc" description="Forward Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(bxc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="bxc" description="Backward Crossed Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(fxc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="fxc" description="Forward Crossed Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(gbc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="gbc" description="Backward Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(gfc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="gfc" description="Forward Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(gbxc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="gbxc" description="Backward Crossed Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(gfxc(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="gfxc" description="Forward Crossed Composition">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(tr(X/(X\Y), D)) :-
  !,
  format('<unaryrule type="ftr" description="Forward Type Raising">'),
  format('<cat>'),
  cat2xml(X/(X\Y)),
  format('</cat>'),
  const2xml(D),
  format('</unaryrule>').
const2xml(tr(X\(X/Y), D)) :-
  !,
  format('<unaryrule type="btr" description="Backward Type Raising">'),
  format('<cat>'),
  cat2xml(X\(X/Y)),
  format('</cat>'),
  const2xml(D),
  format('</unaryrule>').
const2xml(lx(X/(X\Y), Y, D)) :-
  !,
  format('<unaryrule type="ftr" description="Forward Type Raising">'),
  format('<cat>'),
  cat2xml(X/(X\Y)),
  format('</cat>'),
  const2xml(D),
  format('</unaryrule>').
const2xml(lx(X\(X/Y), Y, D)) :-
  !,
  format('<unaryrule type="btr" description="Backward Type Raising">'),
  format('<cat>'),
  cat2xml(X\(X/Y)),
  format('</cat>'),
  const2xml(D),
  format('</unaryrule>').
const2xml(lx(NewCat, _OldCat, D)) :-
  !,
  format('<unaryrule type="tc" description="Type Changing">'),
  format('<cat>'),
  cat2xml(NewCat),
  format('</cat>'),
  const2xml(D),
  format('</unaryrule>').
const2xml(conj(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="conj" description="Conjunction">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(rp(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="rp" description="Remove Punctuation">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(lp(Cat, D1, D2)) :-
  !,
  format('<binaryrule type="lp" description="Left Remove Punctuation">'),
  format('<cat>'),
  cat2xml(Cat),
  format('</cat>'),
  format('<sem><var>nil</var></sem>'),
  const2xml(D1),
  const2xml(D2),
  format('</binaryrule>').
const2xml(Const) :-
  raise_exception(unknown_constituent_type(Const)).

cat2xml(X/Y) :-
  !,
  format('<forward>'),
  cat2xml(X),
  cat2xml(Y),
  format('</forward>').
cat2xml(X\Y) :-
  !,
  format('<backward>'),
  cat2xml(X),
  cat2xml(Y),
  format('</backward>').
cat2xml(Cat:Feat) :-
  !,
  atom_upper(Cat, CAT),
  (  var(Feat)
  -> Feat = 'X'
  ;  true), % HACK
  format('<atomic feature="~w">~w</atomic>', [Feat, CAT]).
cat2xml(conj) :-
  !,
  format('<atomic>conj</atomic>', []).
cat2xml(Cat) :-
  !,
  atom_upper(Cat, CAT),
  format('<atomic>~w</atomic>', [CAT]).
