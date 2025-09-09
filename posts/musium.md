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
I wrote [the flac decoder][claxon],
[loudness analysis][bs1770] and normalization,
and equalizer.
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

## Claxon the flac decoder

Back in 2014,
Rust started to regularly show up in my news feed.
It got many things right that frustrated me in other languages at the time,
and I was eager to learn.
I started by [porting a path tracer][path-tracer],
but what was a good next project?
What’s a good fit for a low-level memory-safe language?
Codecs.
ClusterFuzz did not exist,
and I regularly had media players segfaulting on corrupted files.
I figured that a video codec would be too ambitious,
but audio should be feasible.
So I wrote [Claxon][claxon],
a decoder for the flac codec.
In order to test it and actually play back the result,
I also had to write [Hound][hound],
a library to read and write wav files.
<!--
(It’s called ‘hound’ because ‘waf’ is the sound a dog makes in Dutch,
but ‘dog’ is not a cool name,
so I called it ‘hound’.)
-->
This was the early days of the Rust ecosystem,
and no library for that existed at the time!
It was also pre-1.0,
and just before Rust did
an invasive refactor of IO in the standard library
— a move that Zig would go on to popularize 10 years later.

So I had my flac decoder,
but I didn’t have a _goal_ for it.

[claxon]:      https://github.com/ruuda/claxon
[hound]:       https://github.com/ruuda/hound
[path-tracer]: /2014/08/10/writing-a-path-tracer-in-rust-part-1/
[zero]:        /2016/11/30/zero-cost-abstractions/

## Chromecast and the metadata index

In 2017,
I was subletting a furnished apartment,
and it came with a multi-room Sonos system.
Until then,
at home I mostly listened to music from my PC.
Having music _in every room_ was an amazing new experience,
and I _needed_ to have this at home when I moved back.
Sonos was expensive though,
and the app was unreliable.

Then there was Chromecast Audio.
In theory, it was perfect.
It played flac,
supported multi-room audio,
and I could control it from my phone.
I could even get an employer discount on it,
so I bought three of them.
Chromecast was not a full solution though.
It streams media over http,
but something external needs to trigger playback.
It doesn’t come with a library browser itself.

So that’s what I set out to build:
an http server that could serve my flac files,
with a library browser that would enqueue tracks on the Chromecast.
I had Claxon that could parse tags from flac files,
and I built an application on top of it that would
traverse my flac files at startup,
read their metadata,
and build an in-memory index that it would serve as json.
I intended to run this on a Raspberry Pi,
so I wanted my code to be efficient
— before generation 3, these things were _slow_,
and memory was counted in megabytes.
All of this was of course a premature optimization,
but it was a lot of fun to build.

I couldn’t afford an SSD that could fit my library,
so I kept it on a spinning disk.
Optimizing disk access patterns was a fun journey.
By bumping `/queue/nr_requests` in `/sys/block`,
and reading with hundreds of threads,
the IO scheduler can do a _much_ better job,
and I could index 16k files in 70 seconds with a cold page cache,
down from 130s before optimization.

## Adding a webinterface

## Chromecast is the most unreliable software ever


