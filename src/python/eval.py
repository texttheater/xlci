#!/usr/bin/env python3


import collections
import math
import sys
import util


PUNCTUATION = (',', ':', '!', '?', '.', '"', '“', '”', '„', ',,', "''")


def read_file(path):
    sentence_numbers = set()
    cats = set()
    deps = set()
    with open(path) as f:
        for i, block in enumerate(util.blocks(f)):
            lines = block.splitlines()
            assert lines[-1] == ''
            if len(lines[:-1]) > 0:
                sentence_numbers.add(i)
            for line in lines[:-1]:
                fields = line.split()
                assert len(fields) == 3 or len(fields) == 7
                dep_fromto, dep_token, dep_cat = fields[:3]
                if dep_token in PUNCTUATION:
                    continue
                cats.add((i, dep_fromto, dep_cat))
                if len(fields) == 7:
                    head_fromto, head_token, head_cat, label = fields[3:]
                    deps.add((i, dep_fromto, head_fromto, label))
    return sentence_numbers, cats, deps


def compute(gold, pred):
    tp = gold & pred
    prec = len(tp) / len(pred)
    rec = len(tp) / len(gold)
    try:
        f1 = 2 * prec * rec / (prec + rec)
    except ZeroDivisionError:
        f1 = math.nan
    return prec, rec, f1


def unlabel(dep):
    i, dep_fromto, head_fromto, label = dep
    return (i, dep_fromto, head_fromto, 'DUMMY')


def set2dict(cats):
    result = {}
    for snum, fromto, cat in cats:
        result[(snum, fromto)] = cat
    return result


def compute_confusion(gold_cats, pred_cats):
    gold_dict = set2dict(gold_cats)
    pred_dict = set2dict(pred_cats)
    counter = collections.Counter()
    for coords, gold_cat in gold_dict.items():
        if not coords in pred_dict:
            continue
        pred_cat = pred_dict[coords]
        if gold_cat != pred_cat:
            counter[(gold_cat, pred_cat)] += 1
    return counter


if __name__ == '__main__':
    try:
        _, gold_path, pred_path = sys.argv
    except ValueError:
        print('USAGE: python3 eval.py GOLDFILE PREDICTEDFILE', file=sys.stderr)
    gold_sids, gold_cats, gold_deps = read_file(gold_path)
    pred_sids, pred_cats, pred_deps = read_file(pred_path)
    gold_udeps = set(unlabel(d) for d in gold_deps)
    pred_udeps = set(unlabel(d) for d in pred_deps)
    coverage = len(pred_sids) / len(gold_sids)
    cats_prec, cats_rec, cats_f1 = compute(gold_cats, pred_cats)
    deps_prec, deps_rec, deps_f1 = compute(gold_deps, pred_deps)
    udeps_prec, udeps_rec, udeps_f1 = compute(gold_udeps, pred_udeps)
    cat_confusion = compute_confusion(gold_cats, pred_cats)
    print('Sentence coverage:', coverage)
    print('Category precision:', cats_prec)
    print('Category recall:', cats_rec)
    print('Category f1:', cats_f1)
    print('Labeled dependency precision:', deps_prec)
    print('Labeled dependency recall:', deps_rec)
    print('Labeled dependency f1:', deps_f1)
    print('Unlabeled dependency precision:', udeps_prec)
    print('Unlabeled dependency recall:', udeps_rec)
    print('Unlabeled dependency f1:', udeps_f1)
    print()
    for (gold_cat, pred_cat), frequency in cat_confusion.most_common():
        print('{} {} → {}'.format(frequency, gold_cat, pred_cat))
