#!/usr/bin/env python3


"""Converts plain-text input (on STDIN) to codepoints (1 per line)
"""


import sys


if __name__ == '__main__':
    for line in sys.stdin:
        for char in line:
            print(ord(char))
    print()
