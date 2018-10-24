#!/usr/bin/env python3


"""Strips tashkil diacritics from Arabic text.
"""


import pyarabic.araby as araby
import sys


if __name__ == '__main__':
    for line in sys.stdin:
        print(araby.strip_tashkeel(line), end='')
