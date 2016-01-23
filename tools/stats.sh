#/bin/sh

# Copyright 2016 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

# Run this script from the root of the repository after generating the size to
# print statistics about the size of the generated files.

print_stats()
{
  echo -n "$1"
  find out -name $2 | xargs du -b | sort -n | awk -f tools/stats.awk
}

total_size()
{
  find out -name $1 | xargs du -bc | tail -1 | awk '{print $1}'
}

# Table with stats per file type.

echo        'Type       Median Mean'
echo        '---------- ------ ------'
print_stats 'html      ' '*.html'
print_stats 'all woff  ' '*.woff'
print_stats 'all woff2 ' '*.woff2'
print_stats 'body woff ' '*[0-9]r.woff'
print_stats 'body woff2' '*[0-9]r.woff2'

# Average page weight. (No medians here because matching the html file to the
# font file after generation is hard. No image sizes included because averaging
# the image cost over all pages makes no sense.)

n_pages=$(find out -name '*.html' | wc -l)

html_weight=$(total_size '*.html')
woff_weight=$(total_size '*.woff')
woff2_weight=$(total_size '*.woff2')
image_weight=$(du -bc out/images/* | tail -1 | awk '{print $1}')

woff_page_weight=$(expr $html_weight + $woff_weight)
woff2_page_weight=$(expr $html_weight + $woff2_weight)

echo
echo "Mean page weight (woff):  $(expr $woff_page_weight / $n_pages)"
echo "Mean page weight (woff2): $(expr $woff2_page_weight / $n_pages)"
