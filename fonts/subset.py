# Copyright 2015 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from fontTools.subset import Options, Subsetter, load_font, save_font


def subset(fontfile, text, outfile_basename):
    options = Options()

    # TODO: What is the purpose of the font program table? It is rather large
    # here, making up about a third of the final file size. Apparently it has
    # something to do with hinting. It might have been inserted by the font
    # vendor, one of those automatic "optimisations" ... It appears that
    # without including digits, the table can be removed all right, but the
    # digits require the table to be present.
    # options.drop_tables.append("fpgm")

    # TODO: The glyph names can be stripped, but they might be useful for
    # flattening small caps.
    options.glyph_names = True

    font = load_font(fontfile, options)

    subsetter = Subsetter(options = options)
    subsetter.populate(text = text)
    subsetter.subset(font)

    options.flavor = "woff"
    save_font(font, outfile_basename + ".woff", options)

    options.flavor = "woff2"
    save_font(font, outfile_basename + ".woff2", options)

    font.close()
