#!/usr/bin/env python3


import collections
import re


languages = ('ara', 'ces', 'dan', 'eus', 'nld', 'por', 'slv', 'swe')
lang_devportion = {
    'ara': 'pascal-arabic_dev_15',
    'ces': 'pascal-czech_dev_15',
    'dan': 'pascal-danish_dev_15',
    'eus': 'pascal-basque_dev_15',
    'nld': 'pascal-dutch_dev_15',
    'por': 'pascal-portuguese_dev_15',
    'slv': 'pascal-slovene_dev_15',
    'swe': 'pascal-swedish_dev_15',
}
column_width = 15
ambig_pattern = re.compile(r'^WARNING: (\d+) different parses projected')


def format_int(number):
    """Formats integer number for LaTeX column.

    Thousands separator (thin space), padding."""
    return '{:,}'.format(number).replace(',', '\\,').rjust(column_width)


def format_percentage(number, highlight=False):
    """Formats floating-point number as percentage for LaTeX column.

    Rounding to 1 decimal digit, padding."""
    if highlight:
        fmt = '\\textbf{{{}}}'
    else:
        fmt = '{}'
    return fmt.format('{:.1f}\\%'.format(round(number * 100, 1))).rjust(column_width)


def format_float(number):
    return '{:.3f}'.format(round(number, 3)).rjust(column_width)


if __name__ == '__main__':
    lang_corpussize = {}
    lang_n_projected = collections.defaultdict(dict)
    lang_n_ambiguity = collections.defaultdict(dict)
    lang_n_uas = collections.defaultdict(dict)
    for lang in languages:
        raw_path = 'data/train.{}-eng.src.raw'.format(lang)
        with open(raw_path) as f:
            num_sentences = sum(1 for line in f)
        lang_corpussize[lang] = num_sentences
        for n in range(1, 6):
            # projected
            train_path = 'out/train.{}-eng.trg.proj.{}.feats.parse.tags'.format(lang, n)
            with open(train_path) as f:
                num_projected_derivations = sum(1 for line in f)
            ratio = num_projected_derivations / lang_corpussize[lang]
            lang_n_projected[lang][n] = ratio
            # ambiguity
            log_path = 'out/train.{}-eng.trg.proj.{}.feats.parse.tags.log'.format(lang, n)
            with open(log_path) as f:
                lines = list(f)
            warnings = [int(line.split()[1]) for line in lines if 'different parses projected for' in line]
            total = sum(warnings) + num_projected_derivations - len(warnings)
            ambiguity = total / num_projected_derivations
            lang_n_ambiguity[lang][n] = ambiguity
            # UAS
            eval_path = 'out/{}.{}-eng.trg.xl.{}.feats.eval'.format(lang_devportion[lang], lang, n)
            with open(eval_path) as f:
                for line in f:
                    if line.startswith('Unlabeled dependency recall: '):
                        uas = float(line[29:])
                        break
            lang_n_uas[lang][n] = uas
    print('\\begin{tabular}{llrrrrrrrr}')
    print(' \\toprule')
    print(' language               &          ', end='')
    for lang in languages:
        print(' & ' + '\\multicolumn{{1}}{{c}}{{{}}}'.format(lang).center(column_width), end='')
    print(' \\\\')
    print(' sentence pairs         &          ', end='')
    for lang in languages:
        print(' & ' + format_int(lang_corpussize[lang]), end='')
    print(' \\\\')
    for n in range(1, 6):
        print(' \\midrule')
        print(' \\multirow{{2}}{{*}}{{$n={}$}}'.format(n), end='')
        print(' & projected', end='')
        for lang in languages:
            print(' & ' + format_percentage(lang_n_projected[lang][n]), end='')
        print(' \\\\')
        print('                        & ambiguity', end='')
        for lang in languages:
            print(' & ' + format_float(lang_n_ambiguity[lang][n]), end='')
        print(' \\\\')
        print('                        & UAS      ', end='')
        for lang in languages:
            print(' & ' + format_percentage(lang_n_uas[lang][n],
                                            highlight=(lang_n_uas[lang][n] == max(lang_n_uas[lang].values()))),
                  end='')
        print(' \\\\')
    print(' \\bottomrule')
    print('\\end{tabular}')

