#!/usr/bin/env python3


import collections
import sys
import util


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
                assert len(fields) == 4 or len(fields) == 8
                dep_from, dep_to, dep_token, dep_cat = fields[:4]
                dep_from = int(dep_from)
                dep_to = int(dep_to)
                cats.add((i, dep_from, dep_to, dep_cat))
                if len(fields) == 8:
                    head_from, head_to, head_token, head_cat = fields[4:]
                    deps.add((i, dep_from, dep_to, head_from, head_to))
    return sentence_numbers, cats, deps


def compute(gold, pred):
    tp = gold & pred
    prec = len(tp) / len(pred)
    rec = len(tp) / len(gold)
    f1 = 2 * prec * rec / (prec + rec)
    return prec, rec, f1


def set2dict(cats):
    result = {}
    for snum, fr, to, cat in cats:
        result[(snum, fr, to)] = cat
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
    coverage = len(pred_sids) / len(gold_sids)
    cats_prec, cats_rec, cats_f1 = compute(gold_cats, pred_cats)
    deps_prec, deps_rec, deps_f1 = compute(gold_deps, pred_deps)
    cat_confusion = compute_confusion(gold_cats, pred_cats)
    print('Sentence coverage:', coverage)
    print('Category precision:', cats_prec)
    print('Category recall:', cats_rec)
    print('Category f1:', cats_f1)
    print('Dependency precision:', deps_prec)
    print('Dependency recall:', deps_rec)
    print('Dependency f1:', deps_f1)
    print()
    for (gold_cat, pred_cat), frequency in cat_confusion.most_common():
        print('{} {} → {}'.format(frequency, gold_cat, pred_cat))
