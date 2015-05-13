Personal Site
=============

This is the source code for [my personal site][ruudva].
It is written in Haskell and uses the [Hakyll][hakyll] static site generator.

[![Build Status][ci-img]][ci]

My site was previously a [Jekyll][jekyll] site, and this is a port that tries
to generate exactly the same site. This includes an archive that groups by
year and pagination of the index. Drafts are supported by setting `draft: true`
in the front matter, and math is supported by setting `math: true`. The
JavaScript for rendering math is only included on pages that actually use math.

[ruudva]: https://ruudvanasseldonk.com
[hakyll]: http://jaspervdj.be/hakyll/
[ci-img]: https://travis-ci.org/ruud-v-a/ruudvanasseldonk.com.svg
[ci]:     https://travis-ci.org/ruud-v-a/ruudvanasseldonk.com
[jekyll]: http://jekyllrb.com/

Licence
-------
The source code for this site is licensed under the [GNU General Public Licence][gpl].
See the `licence` file.
The content of the posts is licensed under the [Creative Commons BY SA][cc] licence.

[gpl]: https://gnu.org/licenses/gpl.html
[cc]:  https://creativecommons.org/licenses/by-sa/3.0/

Compiling
---------
Compiling the generator:

```bash
$ cabal update
$ cabal sandbox init
$ cabal install -j
```

Compiling the site:

```bash
$ cabal run build
```

Previewing the site:

```bash
$ cabal run preview
```
