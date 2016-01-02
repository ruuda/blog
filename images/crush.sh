#!/bin/sh

# Before including a new image in the repository, crush it with this script.
# Usage:
#
#     $ ./crush.sh image.png
#
# This will overwrite image.png with the crushed version.

# First apply OptiPNG in best quality but terribly slow mode. This is only run
# once, so it is fine if it takes a few mintues to run.
optipng -o7 $1

# PNGOut can reduce the file size even further in some cases. Most of the
# improvement comes from a custom deflator though, which will be outperformed
# by ZopfliPNG. Still, any byte saved is a win.
pngout $1

# Finally, re-compress the IDAT stream with Zopfli for maximum compression
# ratio. Trade slower compression speed for a better ratio here too.
zopflipng -y --iterations=137 $1 $1
