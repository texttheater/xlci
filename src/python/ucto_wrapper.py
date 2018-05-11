#!/usr/bin/env python3


"""Wrapper for Ucto. Suppresses documents with multiple sentences.
"""


import subprocess
import sys


if __name__ == '__main__':
    for line in sys.stdin.buffer:
        cp = subprocess.run(('ucto', '-c', 'ext/ucto/config/tokconfig-generic'), input=line, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=True)
        #cp = subprocess.run(('ucto'), input=line, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, check=True)
        output = cp.stdout
        num_sentences = output.count(b'<utt>')
        assert num_sentences >= 1
        if num_sentences > 1:
            print()
        else:
            assert output.endswith(b' <utt> \n')
            print(output[:-8].decode('UTF-8'))
