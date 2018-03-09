#!/usr/bin/env python

"""Converts tokenization in IOB format to tokenization in offset format."""

import codecs
import sys
import utils

def start_document():
    global snum, tnum, token, offset
    snum = 0
    tnum = None
    offset = 0
    token = ''

def start_sentence():
    global snum, tnum
    snum += 1
    tnum = 0

def start_token():
    global tnum, token, offset, fr, to
    if tnum == 999:
        raise Exception('A maximum of 999 tokens per sentence is allowed.')
    tnum += 1
    token = ''
    fr = offset
    to = None

def continue_token(code):
    global token, to
    token += unichr(int(code))
    to = None

def finish_token():
    global tnum, fr, to, offset, snum, tnum, token
    # <hack> remove empty tokens
    token = token.strip()
    if token == '':
        tnum -= 1
        return
    # </hack>
    if to == None:
        to = offset
    utils.write_utf8(sys.stdout).write('%d %d %d%03d %s\n' % (fr, to, snum, tnum, token))

start_document()

for line in utils.read_utf8(sys.stdin):
    line = line.rstrip()
    if not line:
        if token:
            finish_token()
        start_document()
        print
        continue
    (code, label) = line.split(' ')
    if code in ('10', '13'): # LF, CR
        code = '32' # SP
    if label == 'S':
        if token:
            finish_token()
        start_sentence()
        start_token()
        continue_token(code)
    elif label == 'T':
        if token:
            finish_token()
        if tnum == None:
            start_sentence()
        start_token()
        continue_token(code)
    elif label == 'I':
        if tnum == None and offset == 0:	#fix for documents that have 'I' as first code
			start_sentence()
			start_token()
        continue_token(code)
    elif label == 'O':
        if snum > 0 and to == None:
            to = offset
    offset += 1

if token:
    finish_token()

