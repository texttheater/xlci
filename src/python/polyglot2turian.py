#!/usr/bin/env python


"""Converts word embeddings from the binary Polyglot
(https://sites.google.com/site/rmyeid/projects/polyglot) format to the
text-based format used by Turian et al. (2010) and EasyCCG.
"""


from __future__ import print_function


import codecs
import numpy
import pickle
import sys


def array2string(array):
    return ' '.join(str(n) for n in array)


if __name__ == '__main__':
    try:
        _, path = sys.argv
    except ValueError:
        print('USAGE: python polyglot2turian.py PKLFILE', file=sys.stderr)
        sys.exit(1)
    with open(path, 'rb') as f:
        words, embeddings = pickle.load(f)
    assert set(words[:4]) == set(('<S>', '</S>', '<PAD>', '<UNK>'))
    for word, embedding in zip(words, embeddings):
        if word == '<UNK>':
            print('*UNKNOWN*', array2string(embedding))
            break
    out = codecs.getwriter('utf8')(sys.stdout)
    for word, embedding in zip(words[4:], embeddings[4:]):
        print(word, array2string(embedding), file=out)
