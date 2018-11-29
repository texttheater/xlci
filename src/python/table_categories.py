#!/usr/bin/env python3


import collections


lang_catfiles = {
    'eng': ('out/train.deu-eng.src.mono.cats',
            'out/train.ita-eng.src.mono.cats',
            'out/train.nld-eng.src.mono.cats',
           ),
    'deu': ('out/train.deu-eng.trg.xl.1.feats.cats',
           ),
    'ita': ('out/train.ita-eng.trg.xl.1.feats.cats',
           ),
    'nld': ('out/train.nld-eng.trg.xl.3.feats.cats',
           ),
}


def latexcat2cat(latexcat):
    return latexcat.replace('\\cat', '').replace('\\?', '\\')


def decfmt(number):
    formatted = '{:.4f}'.format(number)
    if formatted[0] == '0':
        formatted = ' ' + formatted[1:]
    return formatted


if __name__ == '__main__':
    lang_counter = {}
    for lang in lang_catfiles:
        lang_counter[lang] = collections.Counter()
        for catfile in lang_catfiles[lang]:
            with open(catfile) as f:
                lang_counter[lang].update(l.rstrip() for l in f)
    total_eng = sum(lang_counter['eng'].values())
    total_deu = sum(lang_counter['deu'].values())
    total_ita = sum(lang_counter['ita'].values())
    total_nld = sum(lang_counter['nld'].values())
    def numbers(latexcat):
        cat = latexcat2cat(latexcat)
        ratio_eng = lang_counter['eng'][cat] / total_eng
        ratio_deu = lang_counter['deu'][cat] / total_deu
        ratio_ita = lang_counter['ita'][cat] / total_ita
        ratio_nld = lang_counter['nld'][cat] / total_nld
        return '${}$ & {} & {} & {} & {}'.format(latexcat, decfmt(ratio_eng),
            decfmt(ratio_deu), decfmt(ratio_ita), decfmt(ratio_nld))
    print(r'\begin{tabular}{rlcccc}')
    print(r' \toprule')
    print(r'   & Category & English & German & Italian & Dutch \\')
    print(r' \midrule')
    print(r' 1 & {} \\'.format(numbers(r'(\catS[dcl]\?\catNP)/\catNP')))
    print(r'   & {} \\'.format(numbers(r'(\catS[dcl]\?\catNP)\?\catNP')))
    print(r'   & {} \\'.format(numbers(r'(\catS[b]\?\catNP)/\catNP')))
    print(r'   & {} \\'.format(numbers(r'(\catS[b]\?\catNP)\?\catNP')))
    print(r' \midrule')
    print(r' 2 & {} \\'.format(numbers(r'(\catS[dcl]\?\catNP)/(\catS[b]\?\catNP)')))
    print(r' \midrule')
    print(r' 3 & {} \\'.format(numbers(r'(\catS[b]\?\catNP)\?\catPR')))
    print(r'   & {} \\'.format(numbers(r'(\catS[b]\?\catNP)/\catPR')))
    print(r' \midrule')
    print(r' 4 & {} \\'.format(numbers(r'\catN/\catN')))
    print(r'   & {} \\'.format(numbers(r'\catN\?\catN')))
    print(r'   & {} \\'.format(numbers(r'(\catN/\catN)/(\catN/\catN)')))
    print(r'   & {} \\'.format(numbers(r'(\catN\?\catN)/(\catN\?\catN)')))
    print(r' \midrule')
    print(r' 5 & {} \\'.format(numbers(r'\catS[dcl]')))
    print(r'   & {} \\'.format(numbers(r'\catS[dcl]/\catNP')))
    print(r' \bottomrule')
    print(r'\end{tabular}')
