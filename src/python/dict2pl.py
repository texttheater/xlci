#!/usr/bin/env python

"""\
Usage: cat DICTFILE | dict2pl.py N > PLFILE

"""

import argparse
import itertools
import re
import sys

# http://code.activestate.com/recipes/439095-iterator-to-return-items-n-at-a-time/
def group2(iterator, count):
    return itertools.imap(None, *([ iter(iterator) ] * count))

def triple2sentid((comment, trg, ali)):
    return int(COMMENT_PATTERN.match(comment).group(1))

COMMENT_PATTERN = re.compile(r'^# Sentence pair \((\d+)\)')
ALIGN_PATTERN = re.compile(r'(?<=\(\{) ((?:\d+ )*)(?=}\))')

parser = argparse.ArgumentParser(description="""\
Reads GIZA++ output with n-best alignments on STDIN and outputs alignments
from the first up to N alignments as Prolog facts
align(SentNum, SrcWordNums, TrgWordNums).""")
parser.add_argument('-i', dest='invert', action='store_true',
        help='swap source and target side')
parser.add_argument('-n', dest='n', type=int,
        help='use the N-best alignments')
args = parser.parse_args()

for sentence_id, blocks in \
        itertools.groupby(group2(sys.stdin, 3), triple2sentid):
    for alignment_number, (comment, trg, ali) in \
            enumerate(itertools.islice(blocks, args.n), start=1):
        # HACK: keep alignment numbers unique across alignment directions:
        if args.invert:
            alignment_number = - alignment_number
        # Get a list of lists of target token IDs aligned to each source token
        # ID. The first element is the list of unaligned target token IDs, so
        # we ignore that.
        trgid_lists = [[int(i) for i in l.split()] for l in \
                ALIGN_PATTERN.findall(ali)][1:]
        # Output a Prolog term for each source token that is aligned to at
        # least one target token:
        for srcid, trgid_list in enumerate(trgid_lists, start=1):
            if trgid_list:
                srcid_list = [srcid]
                if args.invert:
                    srcid_list, trgid_list = trgid_list, srcid_list
                print('tu({}, {}, {}, {}).'.format(sentence_id, alignment_number,
                        srcid_list, trgid_list))
