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
    """Returns a list of lists of offset pairs."""
    result = []
    with open(path) as f:
        for block in util.blocks(f):
            result.append([])
            for line in block.splitlines():
                if line.rstrip():
                    fr, to, tokid, token = line.split(maxsplit=3)
                    fr = int(fr)
                    to = int(to)
                    result[-1].append((fr, to))
    return result


def read_dict_file(path, nbest_out):
    """Returns a list of pairs (eng_token_count, eng_id_lists)."""
    result = []
    old_sentence_number = 0
    with open(path) as f:
        for comment_line, eng_line, alignment_line in util.chunk(3, f):
            assert comment_line.startswith('# Sentence pair (')
            index = comment_line.index(')')
            sentence_number = int(comment_line[17:index])
            if sentence_number != old_sentence_number:
                assert sentence_number == old_sentence_number + 1
                if sentence_number > 1:
                    result.append(sentence_alignments)
                sentence_alignments = []
            if len(sentence_alignments) < nbest_out:
                old_sentence_number = sentence_number
                eng_token_count = len(eng_line.split())
                eng_id_lists = [[int(i) for i in l.split()] for l
                                in ALIGN_PATTERN.findall(alignment_line)][1:]
                sentence_alignments.append((eng_token_count, eng_id_lists))
    result.append(sentence_alignments)
    return result


if __name__ == '__main__':
    try:
        _, dict_path, engoff_path, foroff_path, nbest_out = sys.argv
        nbest_out = int(nbest_out)
    except ValueError:
        print('USAGE (example): python3 wordalign.py nld-eng.dict eng.tok.off nld.tok.off 3',
              file=sys.stderr)
        sys.exit(1)
    dict_data = read_dict_file(dict_path, nbest_out)
    eng_sentences = read_offset_file(engoff_path)
    for_sentences = read_offset_file(foroff_path)
    assert len(dict_data) == len(eng_sentences)
    assert len(dict_data) == len(for_sentences)
    for alignments, eng_sentence, for_sentence in zip(dict_data, eng_sentences, for_sentences):
        for eng_token_count, eng_id_lists in alignments:
            if eng_token_count != len(eng_sentence) or \
                    len(eng_id_lists) != len(for_sentence):
                print('WARNING: token counts don\'t match, skipping', file=sys.stderr)
            else:
                for (for_from, for_to), eng_id_list in zip(for_sentence, eng_id_lists):
                    print(for_from, for_to, 
                          trgid_list2english_offsets(eng_id_list, eng_sentence),
                          sep ='\t')
        print()
