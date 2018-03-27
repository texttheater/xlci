#!/usr/bin/env python3


import sys
import util


def read_file(path):
    cats = set()
    deps = set()
    with open(path) as f:
        for i, block in enumerate(util.blocks(f)):
            lines = block.splitlines()
            assert lines[-1] == ''
            for line in lines[:-1]:
                fields = line.split()
                assert len(fields) == 4 or len(fields) == 8
                dep_from, dep_to, dep_token, dep_cat = fields[:4]
                cats.add((i, dep_from, dep_to, dep_cat))
                if len(fields) == 8:
                    head_from, head_to, head_token, head_cat = fields[4:]
                    deps.add((i, dep_from, dep_to, head_from, head_to))
    return cats, deps


def compute(gold, pred):
    tp = gold & pred
    prec = len(tp) / len(pred)
    rec = len(tp) / len(gold)
    return prec, rec


if __name__ == '__main__':
    try:
        _, gold_path, pred_path = sys.argv
    except ValueError:
        print('USAGE: python3 eval.py GOLDFILE PREDICTEDFILE', file=sys.stderr)
    gold_cats, gold_deps = read_file(gold_path)
    pred_cats, pred_deps = read_file(pred_path)
    cats_prec, cats_rec = compute(gold_cats, pred_cats)
    deps_prec, deps_rec = compute(gold_deps, pred_deps)
    print('Category precision:', cats_prec)
    print('Category recall:', cats_rec)
    print('Dependency precision:', deps_prec)
    print('Dependency recall:', deps_rec)

