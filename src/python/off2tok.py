#!/usr/bin/env python

import sys
import utils

empty = True
old_snum = 1
out = utils.write_utf8()

for line in utils.read_utf8():
    line = line.strip()
    if line:
        empty = False
        fields = line.split(' ', 3)
        snum = int(fields[2][:-3]) # sentence number
        tnum = int(fields[2][-3:]) # token number
        # if token is an empty string
        if len(fields) == 3:
       	    fields.append('')
        token = fields[3]
        if snum != old_snum:
            out.write('\n') # sentence boundary
            old_snum = snum
        elif tnum != 1:
            sys.stdout.write(' ') # token boundary
        out.write(token.replace(' ', '~'))
    else:
        out.write('\n')
