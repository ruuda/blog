#!/usr/bin/env python3

# Copyright 2015 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from fontTools.subset import Options, Subsetter, load_font, save_font
from sys import stdin

# We expect to be using the pinned version. If it differs, we are probably not
# running from the Nix profile.
import fontTools

assert fontTools.version == "4.51.0", fontTools.version


# Removes format 12 cmap tables if they are not required. A format 4 table is
# always included, but this format can only encode code points in the Basic
# Multilingual Plane (BMP). One code point that I use (U+1D53D, a double-struck
# F), is not in this plane, so if that code point is present, the format 12
# table cannot be removed. In other cases it is a completely redundant copy of
# the format 4 table, so strip it to save space.
def prune_cmaps(font):
    tables = font["cmap"].tables
    min_len = min(len(table.cmap) for table in tables)
    max_len = max(len(table.cmap) for table in tables)

    # Type 12 tables should be a superset of the type 4 tables, so if there is
    # no extra glyph in the type 12 table, the lengths should match.
    if min_len == max_len:
        tables[:] = [table for table in tables if table.format != 12]


def subset(fontfile, outfile_basename, glyphs):
    options = Options()

    # Disable some default-enabled features that we do not use.
    options.layout_features.remove("frac")
    options.layout_features.remove("numr")
    options.layout_features.remove("dnom")

    # There are some dlig glyphs that we want to include.
    options.layout_features.append("dlig")

    # Do not include extra glypths that may be reachable through OpenType
    # features, I specify all the glyphs I want, and nothing more.
    options.layout_closure = False

    # Same for small caps, it needs to be enabled explicitly. Luckily, only the
    # glyphs in the list get included, no extra ones.
    if any(g.endswith(".smcp") for g in glyphs):
        options.layout_features.append("smcp")
        options.layout_features.append("c2sc")

    # Fonts that went through the FontForge roundtrip will have subroutinized
    # programs in the CFF table. This presumably reduces file size for full
    # fonts, but on subsetted fonts it hurts file size and compressability, so
    # desubroutinize.
    options.desubroutinize = True

    # Do not keep the scripts. Older versions of fonttools didn't seem to create
    # a "DFLT" or "latn" script tag in all cases. However, if we remove "latn",
    # then many glyphs (ligatures, smcp) don't get included correctly any more.
    options.layout_scripts = ["latn"]

    # Preserve only name id 1 and 2, the name of the font and the style. Older
    # versions of fonttools preserved only those, newer versions preserve also
    # other ids which contain things like a version number and makeotf writer
    # version, which are pointless to distribute to web clients. (In fact all
    # the name ids are, but let's keep them to have some info about what the
    # font is remaining.)
    options.name_IDs = [1, 2]

    # The "FontForge Time Stamp Table" is useless in our output, delete it.
    options.drop_tables.append("FFTM")

    font = load_font(fontfile, options)

    subsetter = Subsetter(options=options)
    subsetter.populate(glyphs=glyphs)
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
