#!/usr/bin/env python3

# Copyright 2015 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from fontTools.subset import Options, Subsetter, load_font, save_font
from sys import stdin

# Removes format 12 cmap tables if they are not required. A format 4 table is
# always included, but this format can only encode code points in the Basic
# Multilingual Plane (BMP). One code point that I use (U+1D53D, a double-struck
# F), is not in this plane, so if that code point is present, the format 12
# table cannot be removed. In other cases it is a completely redundant copy of
# the format 4 table, so strip it to save space.
def prune_cmaps(font):
    tables = font['cmap'].tables
    min_len = min(len(table.cmap) for table in tables)
    max_len = max(len(table.cmap) for table in tables)

    # Type 12 tables should be a superset of the type 4 tables, so if there is
    # no extra glyph in the type 12 table, the lengths should match.
    if min_len == max_len:
        tables[:] = [table for table in tables if table.format != 12]


def subset(fontfile, outfile_basename, glyphs):
    options = Options()

    # Fonttools has this "feature" that if you enable 'dlig', it will also give
    # you glyphs that you did not ask for, but if you do not enable 'dlig',
    # then discretionary ligatures do not render properly.
    # See https://github.com/behdad/fonttools/issues/43.
    # As a workaround, only enable 'dlig' if there are glyphs for discretionary
    # ligatures.
    dligs = set(glyphs).intersection(['c_b', 'c_h', 'c_k', 'c_p', 'ct', 'g_i',
                                      'q_u', 's_b', 's_h', 's_k', 's_p', 'st'])
    if len(dligs) > 0:
        options.layout_features.append('dlig')
    else:
        # Due to a bug in Fonttools, options are actually global, so the
        # remnants of the previous instance are visible here.
        # See https://github.com/behdad/fonttools/issues/413.
        if 'dlig' in options.layout_features:
            options.layout_features.remove('dlig')

    # Same for small caps, it needs to be enabled explicitly. Luckily, only the
    # glyphs in the list get included, no extra ones.
    if any(g.endswith('.smcp') for g in glyphs):
        options.layout_features.append('smcp')
        options.layout_features.append('c2sc')
    else:
        if 'smcp' in options.layout_features:
            options.layout_features.remove('smcp')
        if 'c2sc' in options.layout_features:
            options.layout_features.remove('c2sc')

    # Fonts that went through the FontForge roundtrip will have subroutinized
    # programs in the CFF table. This presumably reduces file size for full
    # fonts, but on subsetted fonts it hurts file size and compressability, so
    # desubroutinize.
    options.desubroutinize = True

    font = load_font(fontfile, options)

    subsetter = Subsetter(options = options)
    subsetter.populate(glyphs = glyphs)
    subsetter.subset(font)

    prune_cmaps(font)

    options.flavor = "woff"
    save_font(font, outfile_basename + ".woff", options)

    options.flavor = "woff2"
    save_font(font, outfile_basename + ".woff2", options)

    font.close()


# Reads three lines from stdin at a time: the source font file, the destination
# font file basename, and a space-separated list of glyph names to include.
def main():
    while True:
        fontfile = stdin.readline()
        outfile_basename = stdin.readline()
        glyphs = stdin.readline()

        if not fontfile or not outfile_basename or not glyphs:
            break

        glyph_names = glyphs.strip().split(" ")
        subset(fontfile.strip(), outfile_basename.strip(), glyph_names)


main()
