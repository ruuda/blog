#!/bin/sh

# Compresses the resized images to the format that will be served. Because the
# effect of jpeg compression depends a lot on the image, the compression
# settings have been tweaked for every image individually.

# I have a few images where jpeg performs poorly and png performs really well.
# Sometimes the jpeg is slightly smaller than the png at a quality setting
# where the artifacts become unnoticeable, sometimes png actually outperforms
# jpeg.

# Mozjpeg insists on conflicting with libjpeg-turbo, but many packages depend on
# libjpeg-turbo and I don't want to replace it system-wide. Fortunately there
# is an aur package that installs mozjpeg to /opt.
mozjpeg='/opt/mozjpeg/bin/cjpeg -quality'

mkdir -p compressed

$mozjpeg 88 resized/geomancer-soldier-under-attack-by-mage.png > compressed/geomancer-soldier-under-attack-by-mage.jpg
$mozjpeg 88 resized/geomancer-move-mage.png   > compressed/geomancer-move-mage.jpg
$mozjpeg 88 resized/geomancer-overview.png    > compressed/geomancer-overview.jpg
$mozjpeg 99 resized/richys-groceries.png      > compressed/richys-groceries.jpg
$mozjpeg 94 resized/robigo-luculenta.png      > compressed/robigo-luculenta.jpg
$mozjpeg 85 resized/the-small-bang-theory.png > compressed/the-small-bang-theory.jpg

# For this file, at the quality setting where the jpeg artifacts become mostly
# unnoticeable, the jpeg is actually larger than the png, so opt for the png.
cp resized/tristar-cyan.png compressed/tristar-cyan.png
