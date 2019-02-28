xlci2
=====

Code accompanying the paper:

Kilian Evang (2019): Cross-lingual CCG Induction. In *Proceedings of NAACL*.

Set It Up
---------

Some external software is required in the `ext` directory, viz.:

* [`easyccg`](https://github.com/ParallelMeaningBank/easyccg)
* [`mgizapp`](https://github.com/moses-smt/mgiza/tree/master/mgizapp)
* [`torch`](http://torch.ch/docs/getting-started.html#_)

Beyond standard commands available in most Linux distributions, the following
commands need to be available on `$PATH`:

* [`go`](https://golang.org/)
* [`swipl`](http://www.swi-prolog.org/download/stable) (Version 7)
* [`produce`](https://github.com/texttheater/produce)

To evaluate, you need the PASCAL shared task data in a directory at
`data/NAACL`.

Reproduce the Main Experiments
==============================

For example, to run everything on 8 CPU cores:

    produce -j 8 out/pascal-arabic_test_15.ara-eng.trg.xl.2.feats.eval out/pascal-czech_test_15.ces-eng.trg.xl.3.feats.eval out/pascal-danish_test_15.dan-eng.trg.xl.3.feats.eval out/pascal-basque_test_15.eus-eng.trg.xl.5.feats.eval out/pascal-dutch_test_15.nld-eng.trg.xl.2.feats.eval out/pascal-portuguese_test_15.por-eng.trg.xl.1.feats.eval out/pascal-slovene_test_15.trg.xl.4.feats.eval out/pascal-swedish_test_15.swe-eng.trg.xl.1.feats.eval
