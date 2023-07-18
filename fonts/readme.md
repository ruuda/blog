Fonts
=====

I use the [Calluna][calluna] family throughout the site. It is a beautiful
humanist family designed by Jos Buivenga and issued by [Exljbris][exljbris].
The regular sans and serif are available free of charge, but otherwise it is a
commercial family, so for obvious reasons the font files are not included in
this repository.

As monospace font I use [Inconsolata][incons] by Raph Levien. It is inspired
by my all-time favorite coding font Consolas, and it works very well in
combination with Calluna. Inconsolata is a free font licensed under the
[SIL Open Font License][ofl].

[calluna]:  http://www.exljbris.com/calluna.html
[exljbris]: http://www.exljbris.com
[incons]:   http://levien.com/type/myfonts/inconsolata.html
[ofl]:      http://scripts.sil.org/OFL

Extra Glyphs
------------
Calluna does not have glyphs for all the characters that I use on my blog. The
following code points do not have a glyph:

 - U+03C6 Greek small letter phi
 - U+03C8 Greek small letter psi
 - U+220E End of Proof
 - U+2261 Identical to
 - U+1D53D Mathematical double-struck capital F

Browsers fall back to the next font for characters not supported by a font, but
the fallback font used by Chrome on Android does not have the double-struck F,
so I designed my own. For the other glyphs a fallback is acceptable. Perhaps in
the future I’ll roll my own for them too. The FontForge sources for custom
glyphs are in the extra directory.

The script generate.py adds the extra glyphs to the fonts and puts the result in
the generated directory. I don’t mean to imply that the extra glyphs were part
of the original fonts. The reason for doing this is technical: for a font with
a single glyph the metadata overhead is considerable, and the extra font would
unneccessarily complicate my stylesheets and html.

Subsetting
----------
The script subset.py is invoked by the main blog generator to subset the fonts
specifically for every page (see also src/Type.hs). This allows me to do very
aggressive subsetting and keep the file size low.
