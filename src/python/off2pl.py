#!/usr/bin/env python3


import offlib
import sys
import util


def escape(x, singlequote=True):
    result = ''
    for c in x:
        if c in ['\'', '"', '\\'] and (singlequote or c != '\''):
            result += '\\'
        result += c
    return result


if __name__ == '__main__':
    try:
        _, side = sys.argv
    except ValueError:
        print('USAGE: python3 off2pl.py (src|trg)', file=sys.stderr)
        sys.exit(1)
    for snum, block in enumerate(util.blocks(sys.stdin), start=1):
        tuples = offlib.block2tuples(block)
        for fr, to, _, token in tuples:
            print('{}_token({}, {}, {}, \'{}\').'.format(side, snum, fr, to, escape(token)))
