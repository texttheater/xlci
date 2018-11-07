#!/usr/bin/env python3


"""Keeps only sentences up to the specified length.

Punctuation is not counted for determining the length.

Assumes the JSON lines format of Yonatan Bisk's CCG-Induction suite.
"""


PUNCTUATION = (',', ':', '!', '?', '.', '"', '“', '”', '„', ',,', "''")


import json
import sys
import util


if __name__ == '__main__':
    try:
        _, length = sys.argv
        length = int(length)
    except ValueError:
        print('USAGE: python3 sentlen_jsonl.py INT', file=sys.stderr)
        sys.exit(1)
    for line in sys.stdin:
        data = json.loads(line)
        nonpunct_tokens = [w for w in data['words'] if w['word'] not in PUNCTUATION]
        if len(nonpunct_tokens) <= length:
            print(line, end='')
