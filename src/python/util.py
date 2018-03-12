from __future__ import print_function


import sys


def blocks(f):
    """Reads a file into a sequence of blocks.

    Takes a file-like object and returns its contents as a sequence of blocks
    terminated by empty lines."""
    block = ''
    for line in f:
        block += line
        if line == '\n':
            yield block
            block = ''
    if block:
        yield block


def chunk(length, seq):
    """Splits a sequence into fixed-length tuples.
    """
    it = iter(seq)
    while True:
        chunk = []
        for i in range(length):
            try:
                chunk.append(next(it))
            except StopIteration:
                if chunk:
                    yield tuple(chunk)
                return
        yield tuple(chunk)


def monitor_sequence(seq):
    for element in seq:
        print(element, file=sys.stderr)
        yield element
