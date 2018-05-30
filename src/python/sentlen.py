#!/usr/bin/env python3


"""Keeps only sentences with the exact number of tokens specified.

Assumes a CoNLL-X-like format: sentences are terminated by blank lines, one
per line.
"""


PUNCTUATION = (',', ':', '!', '?', '.', '"', '“', '”', '„', ',,', "''")


import sys
import util


def is_nonpunct(line):
    fields = line.split()
    return fields[1] not in PUNCTUATION


if __name__ == '__main__':
    try:
        _, length = sys.argv
        length = int(length)
    except ValueError:
        print('USAGE: python3 sentlen.py INT', file=sys.stderr)
        sys.exit(1)
    for block in util.blocks(sys.stdin):
        lines = block.splitlines()
        assert lines[-1] == ''
        lines = lines[:-1]
        nonpunct_lines = [line for line in lines if is_nonpunct(line)]
        if len(nonpunct_lines) <= length:
            print(block, end='')
