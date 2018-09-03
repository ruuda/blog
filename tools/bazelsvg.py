#!/usr/bin/env python3

# Simple script that parses the output of "bazel analyze-profile --dump=raw"
# and prints an svg bar chart to stdout.

# Copyright 2018 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.


import sys
from typing import Dict, List, NamedTuple, Set

def ns_to_ds(time_ns: int) -> int:
  """Return time rounded to deciseconds."""
  return (time_ns + 50_000_000) // 100_000_000


class Bar(NamedTuple):
  x: int
  y: int
  w: int

# Critical path information is all the way at the end, and does not reference
# the actions by id. We cross-reference them by description instead.
def get_critical_path(fname: str) -> Set[str]:
  with open(fname, 'r') as f:
    critical_path_id = ''
    descriptions: Set[str] = set()

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


def render_bars(fname: str, start_y: int) -> List[str]:
  # Collect bars in a dictionary to deduplicate them, there was some overlap
  # that prevented the critical path from being visible because other bars were
  # on top. The dictionary deduplicates them. Value is the color of the bar.
  bars: Dict[Bar, str] = {}

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

      if ttype in {'SKYFRAME_EVAL', 'CREATE_PACKAGE', 'SKYFUNCTION', 'SKYLARK_USER_FN'}:
        continue

      start_ns = int(start_ns)
      start_dsec = ns_to_ds(start_ns)
      excess_ns = start_ns - start_dsec * 100_000_000

      duration_dsec = ns_to_ds(int(duration_ns) - excess_ns)
      if duration_dsec <= 3:
        continue

      start_dsec = ns_to_ds(int(start_ns))

      if start_x is not None:
        start_dsec -= start_x
      else:
        start_x = start_dsec
        start_dsec = 0

      if thread_id not in tids:
        tids[thread_id] = tid_i % 8
        tid_i += 1
      tid = tids[thread_id]

      is_on_critical_path = description.strip() in critical_path

      # Deduplicate bars by coordinates. The critical path takes priority.
      bar = Bar(start_dsec, 7 - tid, duration_dsec)
      if is_on_critical_path:
        bars[bar] = 'fill="#c35"'
      elif bar not in bars:
        bars[bar] = 'class="bar"'

  return [
    f'<rect x="{bar.x:.0f}" y="{start_y + bar.y * 28:.0f}" '
    f'width="{bar.w:.0f}" height="14" {color}/>'
    for bar, color in sorted(bars.items())
  ]

fname_before, fname_after = sys.argv[1:]
bars_before = render_bars(fname_before, 7)
bars_after = render_bars(fname_after, 287)

# We show 144 seconds in the graph, the last target finishes at 141.7 seconds,
# but 144 is a multiple of 36. # On max width the image is 36em wide, which
# means that 4 units in the svg coordinate system are 1 em. The line height is
# 1.4em, and I want to fit two bars on a line, so I should make every bar 2.8
# high. Also multiply everything by 10, so we don't have to include the decimal
# dot, that saves a few bytes.
print(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="-9.5 0 1440 564">', end='')

# Minified style sheet template that will be rendered by the generator to
# substitute the font hashes. This way we can refer to the same font as the html
# body.
print(
  "<style>"
  "@font-face{font-family:Calluna Sans;"
  'src:url(/fonts/r{{sans-roman-hash}}.woff2)format("woff2"),'
  'url(/fonts/r{{sans-roman-hash}}.woff)format("woff")}'
  ".bar{fill:#b4aaaa}"
  ".label{font:40px 'Calluna Sans',sans-serif;font-variant-numeric:oldstyle-nums;fill:#b4aaaa;text-anchor:middle}"
  "</style>",
  end=''
)

for t in range(0, 144, 10):
  # Lines in my math.css are 0.08em, those go well with the text stroke width,
  # and 1 em is 4 units, so the stroke width for the time grid is 0.32 units.
  print(f'<line x1="{t * 10}" y1="0" x2="{t * 10}" y2="504" stroke="#eee" stroke-width="3.2"/>', end='')

for bar in bars_before + bars_after:
  print(bar, end='')

for t in range(0, 144, 10):
  print(f'<text x="{t * 10}" y="550" class="label">{t}</text>', end='')

print('</svg>', end='')
