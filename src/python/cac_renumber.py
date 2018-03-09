#!/usr/bin/env python


import re
import sys
import utils


if __name__ == '__main__':
    start_pattern = re.compile(r'^ccg\(\d+,')
    next_number = 0 # start at 0 because CAC files start with a header block
    for block in utils.blocks(sys.stdin):
        print start_pattern.sub('ccg({},'.format(next_number), block, 1),
        next_number += 1
