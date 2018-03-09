"""A library for processing tokenization and sentence boundary information. That
information for a text is represented as a pair (tokens, sboundaries) where
tokens is a set of pairs (fr, to) of token offsets and sboundaries is a set of
offsets indicating where sentences end."""

import codecs
import operator
import sys

def tokid(snum, tnum):
   if tnum > 999:
       raise Exception('More than 999 tokens in a sentence are not supported.')
   return '%d%03d' % (snum, tnum)

def snumtnum(tokid):
    return (int(tokid[:-3]), int(tokid[-3:]))

def token_by_id(tokid, tokens):
    (target_snum, target_tnum) = snumtnum(tokid)
    for t in [(fr, to) for (fr, to, snum, tnum) in tokens if snum == \
            target_snum and tnum == target_tnum]:
        return t

class NoSuchTokenError(Exception):
    pass

"""Input: a start offset and an end offset, together with a tokenization
represented as a list of quadruples as returned as the first element of the
pair returned by read_offset_file with ids=True. Output: (snum, tnum)"""
def lookup(target_fr, target_to, tokens):
    for (fr, to, snum, tnum) in tokens:
        if fr == target_fr and to == target_to:
            return (snum, tnum)
    raise NoSuchTokenError()


def _between_exclusive(offset, (fr, to)):
    return offset > fr and offset < to

def _between_inclusive(offset, (fr, to)):
    return offset >= fr and offset <= to

def _expansion(offset, raw_text):
    return (len(raw_text[:offset].rstrip()), len(raw_text) - \
            len(raw_text[offset:].lstrip()))

def _collapses(offset1, offset2, raw_text):
    return _between_inclusive(offset1, _expansion(offset2, raw_text))

"""If ids==true, tokens are represented as quadruples (fr, to, snum, tnum)
rather than pairs (fr, to), where snum is the sentence number and tnum is the
token number within the sentence."""
def read_offset_file(filename, ids=False, toknums=False):
    tokens = set()
    sentence_to = {}
    toknum = 0
    with codecs.open(filename, mode='r', encoding='utf-8') as file:
        for line in file:
            line = line.strip()
            if line:
                toknum += 1
                fields = line.split(' ', 4)
                fr = int(fields[0])
                to = int(fields[1])
                snum = int(fields[2][:-3]) # sentence number
                if toknums:
                    tokens.add((fr, to, toknum))
                elif ids:
                    tnum = int(fields[2][-3:]) # token number
                    tokens.add((fr, to, snum, tnum))
                else:
                    tokens.add((fr, to))
                if not snum in sentence_to:
                    sentence_to[snum] = 0
                sentence_to[snum] = max(sentence_to[snum], to)
    boundaries = set()
    for snum in sentence_to:
        boundaries.add(sentence_to[snum])
    return (tokens, boundaries)

def read_offset_pairs(offset_filename):
    with open(offset_filename) as f:
        for line in f:
            fields = line.split(' ', 4)
            fr = int(fields[0])
            to = int(fields[1])
            yield (fr, to)

def fix_tokenboundaryat(boundary, (tokens, sboundbaries), raw_text):
    expansion = _expansion(boundary, raw_text)
    for (fr, to) in [t for t in tokens if _between_exclusive(boundary, t)]:
        tokens.remove((fr, to))
        tokens.add((fr, expansion[0]))
        tokens.add((expansion[1], to))

def fix_notokenboundaryat(boundary, (tokens, sboundaries), raw_text):
    fix_nosentenceboundaryat(boundary, (tokens, sboundaries), raw_text)
    new_fr = None
    new_to = None
    for (fr, to) in [(fr, to) for (fr, to) in tokens if _collapses(to, \
            boundary, raw_text)]:
        left = (fr, to)
        new_fr = fr
    for (fr, to) in [(fr, to) for (fr, to) in tokens if _collapses(fr, \
            boundary, raw_text)]:
        right = (fr, to)
        new_to = to
    if (new_fr != None and new_to != None):
        tokens.remove(left)
        tokens.remove(right)
        tokens.add((new_fr, new_to))

def fix_sentenceboundaryat(boundary, (tokens, sboundaries), raw_text):
    fix_tokenboundaryat(boundary, (tokens, sboundaries), raw_text)
    for to in [to for (fr, to) in tokens if _collapses(to, boundary, \
        raw_text)]:
        sboundaries.add(to)

def fix_nosentenceboundaryat(boundary, (tokens, sboundaries), raw_text):
    for sboundary in [s for s in sboundaries if _collapses(s, boundary, \
            raw_text)]:
        sboundaries.remove(sboundary)

def _fix_tokens(tokens, raw_text):
    """Splits tokens that contain whitespace."""
    toremove = []
    toadd = []
    for (fr, to) in tokens:
        toremove.append((fr, to))
        newfr = fr
        newto = newfr
        for character in raw_text[fr:to]:
            if character in [u' ', u'\n']:
                if newfr != newto:
                    toadd.append((newfr, newto))
                newfr = newto + 1
                newto = newfr
            else:
                newto += 1
        if newto != newfr:
            toadd.append((newfr, newto))
    for token in toremove:
        tokens.remove(token)
    for token in toadd:
        tokens.add(token)

def write_offset_file((tokens, boundaries), raw_text, filename):
    _fix_tokens(tokens, raw_text)
    tokens_list = list(tokens)
    tokens_list.sort(key=operator.itemgetter(0))
    snum = 1
    tnum = 1
    with codecs.open(filename, mode='w', encoding='utf-8') as file:
        for (fr, to) in tokens_list:
            file.write(unicode(fr))
            file.write(u' ')
            file.write(unicode(to))
            file.write(u' ')
            file.write(unicode(snum))
            if tnum > 999:
                raise Exception('Offset format supports only 999 \
tokens per sentence.')
            file.write(u'%03d' % tnum)
            file.write(u' ')
            file.write(raw_text[fr:to].replace('\n', ' '))
            file.write(u'\n')
            if to in boundaries:
                snum += 1
                tnum = 1
            else:
                tnum += 1

def read_tokens_from_offset_file(filename):
    with codecs.open(filename, 'r', encoding='UTF-8') as offset_file:
        for line in offset_file:
            line = line.rstrip()
            yield line.split(' ', 3)[3]
