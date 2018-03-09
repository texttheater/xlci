#!/usr/bin/env python3


import sys
import util


if __name__ == '__main__':
    for block in util.blocks(sys.stdin):
        lines = block.splitlines()
        if any('S' in line for line in lines[1:]):
            print()
        else:
            print(block, end='')
