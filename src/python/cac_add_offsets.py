#!/usr/bin/env python


from __future__ import print_function


import caclib
import offlib
import sys
import util
import utils


if __name__ == '__main__':
    try:
        _, off_path = sys.argv
    except ValueError:
        print('USAGE: cat FILE.cac | python cac_add_offsets.py FILE.tok.off',
              file=sys.stderr)
        sys.exit(1)
    with open(off_path) as f:
        blocks = [offlib.block2tuples(block) for block in util.blocks(utils.read_utf8(f))]
    snum = 0
    out = utils.write_utf8()
    for line in utils.read_utf8():
        new_snum = caclib.snum(line)
        if new_snum:
            snum = new_snum
            tnum = 0
        token = caclib.token(line)
        if token:
            tnum += 1
            block = blocks[snum - 1]
            tup = block[tnum - 1]
            fr = tup[0]
            to = tup[1]
            token.tags['from'] = fr
            token.tags['to'] = to
            line = token.line()
        out.write(line)
