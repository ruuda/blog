#!/usr/bin/env python3

# Simple script that parses the output of "bazel analyze-profile --dump=raw"
# and prints an svg bar chart to stdout.

# Copyright 2018 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.


import sys

tids = {}
tid_i = 0
ns_per_sec = 1000 * 1000 * 1000
start_x = None

print('<svg xmlns="http://www.w3.org/2000/svg" width="120" height="8">', end='')

for line in sys.stdin:
    thread_id, task_id, parent_id, start_ns, duration_ns, stats, ttype, _ = line.split('|')

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
        tids[thread_id] = tid_i
        tid_i += 1
    tid = tids[thread_id]

    print(f'<rect x="{start_sec:0.1f}" y="{tid}" width="{duration_sec:0.1f}" '
          f'height="0.8" fill="#c35"/>', end='')

print('</svg>', end='')
