#!/usr/bin/env python3


"""Computes average sentence length.

Reads a .tok-format corpus on sys.stdin and outputs the average sentence length
(including punctuation).
"""


import sys


if __name__ == '__main__':
    lengths = []
    for line in sys.stdin:
        lengths.append(len(line.split()))
    print(sum(lengths) / len(lengths))

