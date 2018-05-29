#!/usr/bin/env python3


import sys
import util


if __name__ == '__main__':
    for block in util.blocks(sys.stdin):
        block = block.splitlines()
        block = block[:-1]
        lines = [line for line in block if not line.startswith('#')]
        lines = [line.split() for line in lines]
        forms = []
        for i, line in enumerate(lines, start = 1):
            assert i == int(line[0])
            forms.append(line[1])
        for line in lines:
            dtoknum = int(line[0])
            dform = line[1]
            dcat = '_'
            htoknum = int(line[6])
            hform = forms[htoknum - 1]
            hcat = '_'
            label = '_'
            print('{}\t{}\t{}\t{}\t{}\t{}\t{}'.format(dtoknum, dform, dcat, htoknum, hform, hcat, label))
        print()
