#!/usr/bin/env python3


"""Removes sentences from a .tok.off file that are not in the corresponding .tok file.
"""


import sys
import util


def block2tokens(block):
    lines = block.rstrip().splitlines()
    return [line.split(maxsplit=3)[3].replace(' ', '~') for line in lines]


if __name__ == '__main__':
    try:
        _, tok, off = sys.argv
    except ValueError:
        print('USAGE: python3 off_filter.py FILE.tok FILE.tok.off', file=sys.stderr)
        sys.exit(1)
    with open(off) as f:
        off_blocks = list(util.blocks(f))
    with open(tok) as f:
        tok_lines = list(f)
    assert len(tok_lines) <= len(off_blocks)
    off_index = 0
    for tok_line in tok_lines:
        tok_tokens = tok_line.split()
        while True:
            block = off_blocks[off_index]
            off_tokens = block2tokens(block)
            if tok_tokens == off_tokens:
                print(block, end='')
                break
            else:
                off_index += 1
        off_index += 1
