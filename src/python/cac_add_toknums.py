#!/usr/bin/env python


from __future__ import print_function


import caclib
import offlib
import sys
import util
import utils


if __name__ == '__main__':
    out = utils.write_utf8()
    for line in utils.read_utf8():
        new_snum = caclib.snum(line)
        if new_snum:
            snum = new_snum
            tnum = 0
        token = caclib.token(line)
        if token:
            tnum += 1
            token.tags['toknum'] = tnum
            line = token.line()
        out.write(line)
