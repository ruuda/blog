#!/bin/sh

# Compresses the resized images to the format that will be served. Because the
# effect of jpeg compression depends a lot on the image, the compression
# settings have been tweaked for every image individually.

# I have a few images where jpeg performs poorly and png performs really well.
# Sometimes the jpeg is slightly smaller than the png at a quality setting
# where the artifacts become unnoticeable, sometimes png actually outperforms
# jpeg.

# Fail as soon as any command fails.
set -e

# Verify that we really have Mozjpeg and not regular libjpeg-turbo, as they
# share the same binary name. Mozjpeg prints its version to stderr.
cjpeg -version 2>&1 | grep mozjpeg

# TODO: With mozjpeg 3.1, images were much smaller at the same quality settings
# than with later versions. Figure out which settings compress to the same size
# (or quality!).
mozjpeg='cjpeg -quality'

mkdir -p compressed

$mozjpeg 88.0 resized/geomancer-soldier-under-attack-by-mage.png > compressed/geomancer-soldier-under-attack-by-mage.jpg
$mozjpeg 88.0 resized/geomancer-move-mage.png   > compressed/geomancer-move-mage.jpg
$mozjpeg 88.0 resized/geomancer-overview.png    > compressed/geomancer-overview.jpg
$mozjpeg 94.5 resized/robigo-luculenta.png      > compressed/robigo-luculenta.jpg
$mozjpeg 85.0 resized/the-small-bang-theory.png > compressed/the-small-bang-theory.jpg

guetzli --quality 93 resized/richys-groceries.png compressed/richys-groceries.jpg

# For this file, at the quality setting where the jpeg artifacts become mostly
# unnoticeable, the jpeg is actually larger than the png, so opt for the png.
cp resized/tristar-cyan.png compressed/tristar-cyan.png

# Guetzli experiments
# -------------------
# Conclusion: apart from one image, Guetzli is universally worse than Mozjpeg
# for my use case. The main problem is that although it tends to produce less
# artifacts, it produces blocky gradients, which are much worse. My images do
# contain lots of gradients and relatively little high-frequency detail.

# geomancer-soldier-under-attack-by-mage.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         90.0          191   almost acceptable (less artifacts than mozjpeg, but blocky gradients)
# Guetzli         87.0          161   bad (blocky gradients, minor artifacts)
# Guetzli         84.0          146   bad (very blocky gradients, artifacts)
# Mozjpeg         88.0          158   acceptable (minor artifacts along edges)

# geomancer-move-mage.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         90.0          205   almost acceptable (blocky gradients)
# Guetzli         87.0          175   almost acceptable (artifacts along edges rather than blur, blocky gradients)
# Mozjpeg         88.0          176   acceptable (minor artifacts along edges)

# geomancer-overview.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         90.0          107   acceptable
# Guetzli         87.0           84   acceptable, but slightly worse than mozjpeg in edge artifacts
# Guetzli         86.0           82   almost acceptable (still noticeable artifacts along edges)
# Guetzli         85.0           78   bad (artifacts along edges)
# Mozjpeg         88.0           84   acceptable (artifacts in high-detail regions, but not along edges)

# robigo-luculenta.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         95.0          192   good
# Guetzli         93.0          130   good
# Guetzli         92.0          118   almost acceptable (still blocky gradients)
# Guetzli         91.0          111   almost acceptable (still blocky gradients)
# Guetzli         90.0          106   bad (gradients turn blocky)
# Mozjpeg         94.5          126   good

# richys-groceries.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         99.0          400   good
# Guetzli         95.0          241   good
# Guetzli         93.0          187   good
# Guetzli         92.0          168   barely acceptable (shows minor artifacts)
# Guetzli         91.0          130   shows artifacts
# Guetzli         90.0          124   shows artifacts
# Mozjpeg         99.0          199   acceptable (parts blurry, but that's ok)

# the-small-bang-theory.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         85.0           57   acceptable (minor artifacts)
# Guetzli         84.0           55   bad (noticeable artifacts around text)
# Mozjpeg         85.0           33   acceptable (minor artifacts)

# tristar-cyan.png
# Compressor   Quality   Size (KiB)   Result
# Guetzli         90.0          150   almost acceptable (artifacts)
# Guetzli         84.0          124   bad (artifacts)
# Mozjpeg         99.0          278   good
# Mozjpeg         90.0          116   bad (artifacts)
# Mozjpeg         89.5           83   bad (artifacts)
# Mozjpeg         89.0           79   bad (artifacts)
# Mozjpeg         80.0           57   terrible (artifacts)
# None (png)                    108   perfect
