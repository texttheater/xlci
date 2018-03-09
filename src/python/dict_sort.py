#!/usr/bin/env python3

"""
Merge alignments scattered over several output files by MGIZA++ back into the
correct order.
"""

import collections
import re
import sys

PAIR_PATTERN = re.compile(r'^# Sentence pair \((\d+)\)')

alignments = collections.defaultdict(lambda: '')

while True:
    try:
        alignment = next(sys.stdin)
    except StopIteration:
        break
    alignment += next(sys.stdin)
    alignment += next(sys.stdin)
    match = PAIR_PATTERN.match(alignment)
    alignments[int(match.group(1))] += alignment

for i in range(1, max(alignments.keys()) + 1):
    print(alignments[i], end='')
