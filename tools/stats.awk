# Copyright 2016 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

# This script is intended to be invoked from stats.sh.

{
  size[i++] = $1;
  total += $1;
}
END {
  mean = total / i;

  if (i % 2)
  {
    median = size[int(i / 2)];
  }
  else
  {
    median = size[i / 2] + size[i / 2 + i];
  }

  printf(" %6.f %6.f\n", median, mean);
}
