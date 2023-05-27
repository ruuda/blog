---
title: An algorithm for shuffling playlists
date: 2023-05-27
minutes: ?
synopsis: TODO
run-in: TODO
---

It is widely know that for shuffling playlists,
humans don’t want an _actual_ shuffle
drawn uniformly from all possible permutations of the playlist,
because that might play the same artist twice in a row,
which doesn’t feel shuffled.
Much has been written about this topic,
including famously [by Spotify][spotify],
which in turn references [Martin Fiedler’s algorithm][martin].
However, neither algorithm is optimal,
in the sense that they might play an artist consecutively
in cases where this was possible to avoid.
I’ve been thinking about this for Musium,
the music player that I am building,
and in this post I want to outline an algorithm that is optimal in the above sense.

[martin]: https://keyj.emphy.de/balanced-shuffle/
[spotify]: https://engineering.atspotify.com/2014/02/how-to-shuffle-songs/
[musium]: https://github.com/ruuda/musium

Inverleaving
------------