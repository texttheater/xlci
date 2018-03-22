#!/usr/bin/env python3


"""Filters word alignments corresponding to the subset of a corpus.

Reads a parallel corpus, corresponding word alignments and a subset of the
parallel corpus and outputs only the word alignments corresponding to the
subset.

Corpus and subset are given as files with one sentence pair per line (in
whatever format as long as it's consistent). Word alignments are given and
output as a file of blocks (sequences of lines separated by empty lines -
beyond that, the format does not matter).
"""


import sys
import util


if __name__ == '__main__':
    try:
        _, corpus_path, align_path, subset_path = sys.argv
    except ValueError:
        print('USAGE (example): python3 wordalign_pick.py '
              'proj1.nld-eng.ftok proj1.nld-eng.wordalign dev.nld-eng.ftok',
              file=sys.stderr)
        sys.exit(1)
    with open(corpus_path) as f:
        corpus = list(f)
    with open(align_path) as f:
        alignments = list(util.blocks(f))
    with open(subset_path) as f:
        subset = list(f)
    assert len(corpus) == len(alignments)
    assert set(subset) < set(corpus)
    pair_alignment_dict = dict(zip(corpus, alignments))
    for pair in subset:
        print(pair_alignment_dict[pair], end='')
