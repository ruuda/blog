#!/usr/bin/env python3

# Copyright 2016 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

import fontforge
import fontTools.subset as fonttools
import os


# Note: roundtripping an otf font through FontForge (with open and generate) is
# not a lossless process, and it only appears to reach a fixed point after the
# third roundtrip. One difference after roundtripping is that FontForge adds an
# FFTM table with timestamps of last modification. It also adds a GDEF table.
# Some other tables (name, cmap, CFF, and GSUB) are modified as well, but I do
# not know in what manner. The changes do not appear to be significant for
# functionality, but they do increase the size of the fonts.
#
# The difference in the name table is not relevant, because after woff
# compression the tables are the same again.


# Removes some unnecessary data from an otf font.
def prune_font(fontfile, out_dir):
    font = fonttools.load_font(fontfile, fonttools.Options())

    # Roundtripping a font trough FontForge adds a GDEF table. This table is not
    # present in the original version of Calluna, so remove it. It is present in
    # Inconsolata, but it does not appear to do any harm to remove it, apart
    # from reducing the file size.
    if 'GDEF' in font:
        del font['GDEF']

    font.save(os.path.join(out_dir, os.path.basename(fontfile)))
    font.close()


def main():
    os.makedirs('generated/', exist_ok = True)

    # Merge math-upright (which contains U+1D53D, a double-struck F)
    # into Calluna Sans.
    calluna_sans = fontforge.open('original/calluna-sans.otf')
    calluna_sans.mergeFonts('extra/math-upright.sfd')
    calluna_sans.generate('generated/calluna-sans.otf', flags = 'opentype')
    calluna_sans.close()

    prune_font('generated/calluna-sans.otf', 'generated/')

    # Just copy over the other fonts, prune them in the process.
    prune_font('original/calluna-bold.otf',        'generated/')
    prune_font('original/calluna-italic.otf',      'generated/')
    prune_font('original/calluna.otf',             'generated/')
    prune_font('original/calluna-sans-bold.otf',   'generated/')
    prune_font('original/calluna-sans-italic.otf', 'generated/')
    prune_font('original/inconsolata.otf',         'generated/')


main()
