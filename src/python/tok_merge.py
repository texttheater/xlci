#!/usr/bin/env python3


"""Merges two .tok files into a parallel corpus.

Discards sentences that are missing on either side. Makes sure the whole file
lines up.
"""


import sys


if __name__ == '__main__':
    try:
        _, file1, file2 = sys.argv
    except ValueError:
        print('USAGE: python3 tok_merg.pye FILE1 FILE2', file=sys.stderr)
    with open(file1) as f:
        lines1 = f.readlines()
    with open(file2) as f:
        lines2 = f.readlines()
    assert len(lines1) == len(lines2)
    for line1, line2 in zip(lines1, lines2):
        if not line1.rstrip():
            continue
        if not line2.rstrip():
            continue
        assert '\t' not in line1
        assert '\t' not in line2
        print(line1.rstrip(), line2.rstrip(), sep='\t')
