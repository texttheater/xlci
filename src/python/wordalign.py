#!/usr/bin/env python3


"""Converts (M)GIZA++ output to a token-offset-based format.

Input example:

# Sentence pair (33977) source length 7 target length 8 alignment score : 4.28365e-07
can you give me your phone number ?
NULL ({ }) kan ({ 1 }) je ({ 2 }) mij ({ 4 }) je ({ 5 }) telefoonnummer ({ 6 7 }) geven ({ 3 }) ? ({ 8 }) 

Assuming the following raw texts:

Can you give me your phone number?

Kan je mij je telefoonnummer geven?

Output example:

0	3	0,3
4	6	4,7
7	10	13,15
11	13	16,20
14	28	21,16 27,33
29	34	8,12
34	35	33,34
"""


import re
import sys
import util


# Matches a GIZA++ alignment line and extracts the bits between ({ and }).
ALIGN_PATTERN = re.compile(r'(?<=\(\{) ((?:\d+ )*)(?=}\))')


def trgid_list2english_offsets(trgid_list, english_sentence):
    english_offsets = []
    for trgid in trgid_list:
        eng_from = english_sentence[trgid - 1][0]
        eng_to = english_sentence[trgid - 1][1]
        english_offsets.append((eng_from, eng_to))
    english_offsets = ['{},{}'.format(*p) for p in english_offsets]
    english_offsets = ' '.join(english_offsets)
    return english_offsets


def read_offset_file(path):
    """Returns a flat list of offset pairs."""
    result = []
    with open(path) as f:
        for line in f:
            if line.rstrip():
                fr, to, tokid, token = line.split(maxsplit=3)
                fr = int(fr)
                to = int(to)
                result.append((fr, to))
    return result


if __name__ == '__main__':
    try:
        _, dict_path, engoff_path, foroff_path = sys.argv
    except ValueError:
        print('USAGE (example): python3 wordalign.py nld-eng.dict eng.tok.off nld.tok.off',
              file=sys.stderr)
        sys.exit(1)
    eng_pairs = read_offset_file(engoff_path)
    for_pairs = read_offset_file(foroff_path)
    with open(dict_path) as f:
        for comment_line, eng_line, alignment_line in util.chunk(3, f):
            eng_tokens_count = len(eng_line.split())
            if eng_tokens_count > 100:
                print('WARNING: sentence too long for GIZA++, skipping', file=sys.stderr)
            else:
                # Get a list of lists of target token IDs aligned to each
                # source token ID. The first element is the list of unaligned
                # target token IDs, so we ignore that.
                eng_id_lists = [[int(i) for i in l.split()] for l
                                in ALIGN_PATTERN.findall(alignment_line)][1:]
                for_tokens_count = len(eng_id_lists)
                eng_pairs_sentence = eng_pairs[:eng_tokens_count]
                del eng_pairs[:eng_tokens_count]
                for_pairs_sentence = for_pairs[:for_tokens_count]
                del for_pairs[:for_tokens_count]
                for for_pair, eng_id_list in zip(for_pairs_sentence, eng_id_lists):
                    print(for_pair[0], for_pair[1], 
                          trgid_list2english_offsets(eng_id_list, eng_pairs_sentence),
                          sep ='\t')
            print()
    assert len(eng_pairs) == 0
    assert len(for_pairs) == 0
