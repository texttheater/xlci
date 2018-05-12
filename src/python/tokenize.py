#!/usr/bin/env python3


"""Wrapper for Ucto. Suppresses documents with multiple sentences.
"""


import sys
import ucto


if __name__ == '__main__':
    try:
        _, config_path = sys.argv
    except ValueError:
        print('USAGE: python3 tokenize.py UCTO_CONFIG_FILE', file=sys.stderr)
        sys.exit(1)
    tokenizer = ucto.Tokenizer(config_path)
    for line in sys.stdin:
        tokenizer.process(line)
        tokens = list(tokenizer)
        if len(tokens) == 0:
            print()
            continue
        if any(t.isendofsentence() for t in tokens[:-1]):
            # suppress document with multiple sentences
            print()
            continue
        print(' '.join(str(t) for t in tokens))
