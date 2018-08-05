#!/usr/bin/env python3

# Simple script that parses the output of "bazel analyze-profile --dump=raw"
# and prints an svg bar chart to stdout.

# Copyright 2018 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.


import sys
from typing import List

ns_per_sec = 1000 * 1000 * 1000
max_sec = 0

# Critical path information is all the way at the end, and does not reference
# the actions by id. We cross-reference them by description instead.
def get_critical_path(fname: str):
  with open(fname, 'r') as f:
    critical_path_id = ''
    descriptions = set()
    for line in f:
      thread_id, task_id, parent_id, start_ns, duration_ns, stats, ttype, description = line.split('|')

      if ttype == 'CRITICAL_PATH':
        critical_path_id = task_id

      if parent_id == critical_path_id:
        if description.startswith("action '"):
          # Turn "action 'foobar'\n" into "foobar".
          description = description[len("action '"):-2]
          descriptions.add(description)

  return descriptions


def render_bars(fname: str, start_y: float) -> List[str]:
  global max_sec
  bars = []

  critical_path = get_critical_path(fname)

  with open(fname, 'r') as f:
    tids = {}
    tid_i = 0
    start_x = None

    for line in f:
      thread_id, task_id, parent_id, start_ns, duration_ns, stats, ttype, description = line.split('|')

      # Exclude subtasks because we already draw the parent, apart from the
      # critical path components, which are children of the critical path.
      if parent_id != '0':
        continue

      if ttype == 'SKYFRAME_EVAL':
        continue

      duration_sec = int(duration_ns) / ns_per_sec
      if duration_sec < 0.3:
        continue

      start_sec = int(start_ns) / ns_per_sec

      if start_x is not None:
        start_sec -= start_x
      else:
        start_x = start_sec
        start_sec = 0

      if thread_id not in tids:
        tids[thread_id] = tid_i % 8
        tid_i += 1
      tid = tids[thread_id]

      max_sec = max(max_sec, start_sec + duration_sec)

      description = description.strip()
      color = '#c35' if description in critical_path else '#456';

      bars.append(
          f'<rect x="{start_sec:0.1f}" y="{start_y + tid * 2.8:.1f}" width="{duration_sec:0.1f}" '
        f'height="1.4" fill="{color}"/>'
      )

  return bars

fname_before, fname_after = sys.argv[1:]
bars_before = render_bars(fname_before, 0.7)
bars_after = render_bars(fname_after, 28.7)

# We show 144 seconds in the graph, the last target finishes at 141.7 seconds,
# but 144 is a multiple of 36. # On max width the image is 36em wide, which
# means that 4 units in the svg coordinate system are 1 em. The line height is
# 1.4em, and I want to fit two bars on a line, so I should make every bar 2.8
# high.
print(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="-0.9 0 144 56.4">', end='')
print("<style>.label{font:4px 'Calluna Sans';fill:#bbb;text-anchor:middle}</style>", end='')

for t in range(0, 144, 10):
  # Lines in my math.css are 0.08em, those go well with the text stroke width,
  # and 1 em is 4 units, so the stroke width for the time grid is 0.32 units.
  print(f'<line x1="{t}" y1="0" x2="{t}" y2="50.4" stroke="#ddd" stroke-width="0.32"/>', end='')

for bar in bars_before + bars_after:
  print(bar, end='')

for t in range(0, 144, 10):
  print(f'<text x="{t}" y="55" class="label">{t}</text>', end='')

print('</svg>', end='')
