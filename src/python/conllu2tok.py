#!/usr/bin/env python3


import sys
import util


if __name__ == '__main__':
    for block in util.blocks(sys.stdin):
        block = block.splitlines()
        block = block[:-1]
        block = [line for line in block if not line.startswith('#')]
        tokens = [line.split()[1] for line in block]
        print(' '.join(tokens))
