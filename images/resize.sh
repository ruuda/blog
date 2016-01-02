#!/bin/sh

# Resize an image to be 1280 pixels wide. Keep the same aspect ratio.
# Usage:
#
#     $ ./resize.sh image.png image-1280.png

# Resize in the linear LAB color space, but write the result in sRGB. Do not
# write gamma information in the png metadata though. This feature of png
# causes more trouble than it solves, because it is often handled incorrectly.
# Programs and graphics APIs make wrong assumptions about the color space or
# try to apply corrections at the wrong point. Just strip gamma information and
# hope the entire pipeline is sRGB.

# Sometimes resizing with `-resize` instead of `-distort Resize` can produce a
# smaller output image.

convert $1                       \
  -colorspace LAB                \
  -filter Lanczos2               \
  -distort Resize 1280           \
  -colorspace sRGB               \
  -define png:exclude-chunk=gAMA \
  -define png:exclude-chunk=cHRM \
  $2
