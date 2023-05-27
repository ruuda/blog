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

Inverleaving two artists
------------------------
Suppose we have a playlist with just two artists, A and B,
and they have _n_ and _m_ tracks respectively.
Without loss of generality we may assume that _n_ ≥ _m_.
We can create a playlist without consecutive artists
if and only if _m_ ≥ _n_ - 1.
With _m_ = _n_ - 1
we can put one of B’s track in between each of A’s tracks.
If we had any less,
then some of A’s tracks will need to be consecutive.
With _m_ = _n_ we can also add a track at the start or end.
When _m_ > _n_, that would violate our assumption.

From this it is not so hard to see
how we can optimally shuffle a playlist with two artists:

1. Partition on artist,
   so we have list A of length _n_
   and list B of length _m_,
   with _n_ > _m_.
2. Shuffle the lists internally using a true shuffle.
   (Later we will refine this
   to at least try and avoid consecutive tracks from the same album.)
3. Draw _m_ indices from {0, 1, 2, …, _n_} without replacement.
4. Insert B’s elements at those indices into A.

When possible this will interleave A and B
and avoid consecutive tracks by the same artist.
When consecutive tracks are inevitable,
the algorithm ensures at least
that only one of the two artists has consecutive tracks.
What this algorithm naively does not do,
but Spotify’s algorithm does,
is ensure that the spans by the same artist have roughly the same size.
This can be mitigated by using stratified sampling for step 3:
break the _n_ + 1 indices evenly into _m_ bins,
and sample an index from every bin.

Note, for two artists, Spotify’s algorithm works well.
But they merge in one go instead of merging pairwise,
which creates the problem.