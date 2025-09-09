---
title: The story of Musium
date: 2025-09-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Musium is the music player
---

Musium is the music player that I built for myself.
It runs on a Raspberri Pi,
and I can control it from my local network using a webinterface.
I’ve been using it on a daily basis for years,
but it’s still far from complete.
For example, there is no way to pause or stop playback yet.

<p style="text-align: center">
<img
  src="/images/musium.png"
  alt="The Musium music player."
  style="width: 80%; margin-top: 1em; margin-bottom: 0.5em"
  />
</p>

[Musium][musium] is my ultimate yak shave,
and it’s NIH’d across the stack.
I wrote [the FLAC decoder][claxon],
[loudness analysis][bs1770] and normalization,
and high-pass filter.
The core data structures to index and search the library are custom,
including a specialized hash table.
It persists data to SQLite,
for which I wrote [a code generator][squiller]
that generates Rust bindings for SQL queries.
The frontend is written in PureScript,
for which I wrote a custom web framework.
The seek bar is not just a line, it renders a waveform,
and the UI is animated throughout.
Musium tracks fairly elaborate statistics about playcounts,
so it can surface interesting music at the right time,
and I developed [a new shuffling algorithm][shuffle] for it.
Musium does _exactly_ what I want it to do,
and that’s very satisfying.
This is its story.

[claxon]:   https://github.com/ruuda/claxon
[bs1770]:   https://github.com/ruuda/bs1770
[shuffle]:  /2023/an-algorithm-for-shuffling-playlists
[musium]:   https://github.com/ruuda/musium
[squiller]: https://github.com/ruuda/squiller

