#!/usr/bin/env python3


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


column_width = 8


def format_int(number):
    """Formats integer number for LaTeX column.

    Thousands separator (thin space), padding."""
    return '{:,}'.format(number).replace(',', '\\,').rjust(column_width)


def format_percentage(number):
    """Formats floating-point number as percentage for LaTeX column.

    Rounding to 1 decimal digit, padding."""
    return '{:.1f}\\%'.format(round(number * 100, 1)).rjust(column_width)


if __name__ == '__main__':
    lang_corpussize = {}
    for lang in languages:
        raw_path = 'data/train.{}-eng.src.raw'.format(lang)
        with open(raw_path) as f:
            num_sentences = sum(1 for line in f)
        lang_corpussize[lang] = num_sentences
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
            train_path = 'out/train.{}-eng.trg.proj.{}.feats.parse.tags'.format(lang, n)
            with open(train_path) as f:
                num_projected_derivations = sum(1 for line in f)
            ratio = num_projected_derivations / lang_corpussize[lang]
            print(' & ' + format_percentage(ratio), end='')
        print(' \\\\')
        print('                        & UAS      ', end='')
        for lang in languages:
            eval_path = 'out/{}.{}-eng.trg.xl.{}.feats.eval'.format(lang_devportion[lang], lang, n)
            with open(eval_path) as f:
                for line in f:
                    if line.startswith('Unlabeled dependency recall: '):
                        uas = float(line[29:])
                        break
            print(' & ' + format_percentage(uas), end='')
        print(' \\\\')
    print(' \\bottomrule')
    print('\\end{tabular}')

