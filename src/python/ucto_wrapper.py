#!/usr/bin/env python3


"""Wrapper for Ucto. Suppresses documents with multiple sentences.
"""


import subprocess
import sys


if __name__ == '__main__':
    try:
        _, config_path = sys.argv
    except ValueError:
        print('USAGE: python3 ucto_wrapper.py CONFIGFILE', file=sys.stderr)
        sys.exit(1)
    for line in sys.stdin.buffer:
        cp = subprocess.run(('ucto', '-c', config_path), input=line, stdout=subprocess.PIPE, check=True)
        output = cp.stdout
        num_sentences = output.count(b'<utt>')
        assert num_sentences >= 1
        if num_sentences > 1:
            print()
        else:
            assert output.endswith(b' <utt> \n')
            print(output[:-8].decode('UTF-8'))
