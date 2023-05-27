---
title: An algorithm for shuffling playlists
date: 2023-05-27
minutes: ?
synopsis: TODO
run-in: It is widely known
---

It is widely known that for shuffling playlists,
humans don’t want an _actual_ shuffle,
because that might play the same artist twice in a row,
which doesn’t feel random.
Much has been written about this topic,
including famously [by Spotify][spotify],
which in turn references [Martin Fiedler’s algorithm][martin].
However, neither algorithm is optimal,
in the sense that they might play an artist consecutively
in cases where this was possible to avoid.
I’ve been thinking about this for [Musium][musium],
the music player that I am building.
In this post I want to outline an algorithm that _is_ optimal in the above sense.

[martin]: https://keyj.emphy.de/balanced-shuffle/
[spotify]: https://engineering.atspotify.com/2014/02/how-to-shuffle-songs/
[musium]: https://github.com/ruuda/musium

Inverleaving
------------
Suppose we have a playlist with just two artists, A and B,
and they have _n_ and _m_ tracks respectively.
Without loss of generality we may assume that _n_ ≥ _m_.
We can create a playlist without consecutive tracks
if and only if _m_ ≥ _n_ - 1.
With _m_ = _n_ - 1
we can put one of B’s track in between each of A’s tracks.
If we had any less,
then some of A’s tracks will need to be consecutive.
With _m_ = _n_ we can also add a track at the start or end.
And when _m_ > _n_, that would violate our assumption.
