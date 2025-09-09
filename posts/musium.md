---
title: The story of Musium
date: 2025-09-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Musium is the music player
---

[Musium][musium] is the music player that I built for myself.
It runs on a Raspberri Pi,
and I can control it from my local network using a webinterface.
Musium is NIH’d across the entire stack.
I wrote the FLAC decoder,
loudness analysis and normalization,
and high-pass filter.
The core data structures to index and search the library are custom,
including a specialized hash table.
It persists data to SQLite,
for which I wrote [a code generator][squiller]
that generates statically typed bindings from SQL queries.
The frontend is written in PureScript,
for which I wrote a custom web framework.
It tracks playcounts so it can surface interesting music at the right time,
and I developed [a new shufflingi algorithm][shuffle] for it.
Musium does _exactly_ what I want it to do,
and that’s very satisfying.
Yet, it’s still far from complete.
For example, there is no way to pause or stop playback yet.
I’ve been using it on a daily basis for years.
This is the story of Musium.

[shuffle]:  /2023/an-algorithm-for-shuffling-playlists
[musium]:   https://github.com/ruuda/musium
[squiller]: https://github.com/ruuda/squiller

