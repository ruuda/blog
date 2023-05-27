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
   so we have list <abbr>A</abbr> of length _n_
   and list <abbr>B</abbr> of length _m_,
   with _n_ > _m_.
2. Shuffle the lists internally using a true shuffle.
   (Later we will refine this
   to at least try and avoid consecutive tracks from the same album.)
3. Draw _m_ indices from {1, 2, …, _n_ - 1} without replacement.
   If _m_ = _n_, draw from {0, 1, …, _n_}.
4. Insert <abbr>B</abbr>’s elements in front of those indices into <abbr>A</abbr>.

When possible this will interleave <abbr>A</abbr> and <abbr>B</abbr>
and avoid consecutive tracks by the same artist.
When consecutive tracks are inevitable,
the algorithm ensures at least
that only one of the two artists has consecutive tracks.
What this algorithm naively does not do,
but Spotify’s algorithm does,
is ensure that the spans by the same artist have roughly the same size.
This can be mitigated by using stratified sampling for step 3:
break the indices evenly into _m_ bins,
and sample an index from every bin.

Multiple artists
----------------
Now that we can optimally shuffle two artists,
we can extend the idea to more artists.
Let’s look at an example first.
Say we have four tracks by artist <abbr>A</abbr>,
two by <abbr>B</abbr>, and one by <abbr>C</abbr>.
Then the three optimal shuffles are ABABACA, ABACABA, and ACABABA.
Between <abbr>B</abbr> and <abbr>C</abbr> we have some freedom,
but we need all the <abbr>B</abbr>’s and <abbr>C</abbr>’s
together to go in between the <abbr>A</abbr>’s.
What we did here
is to first interleave <abbr>B</abbr> and <abbr>C</abbr>,
and then we interleaved the result with <abbr>A</abbr>.

If we have an already shuffled list of _n_ tracks
and we want to merge in _m_ more tracks by a new artist not in the list,
then if _n_ ≥ _m_,
we can do this without creating consecutive tracks by the same artist.
In other words,
when we consider a new artist with _m_ tracks,
we should try to have an already-shuffled list of at least _m_ - 1 tracks.

Putting things together,
we get the following incremental algorithm:

1. Partition on artist, shuffle the partitions internally.
2. Initialize the intermediate result to an empty list.
3. While there are partitions left,
   interleave the smallest partition into the intermediate result,
   as in step 3 and 4 of the previous section.

TODO: Always merge the smallest into intermediate,
or merge the smallest two partitions?

By interleaving the smallest partitions first,
we ensure that by the time we get to a larger partition,
we have enough tracks to interleave it with.
If at some point we produced consecutive tracks in the intermediate list,
then interleaving it into a larger partition
will break up the consecutive tracks.

Optimality proof
----------------
_If you are not interested in the formalities you can safely skip this section._

I wonder,
suppose the intermediate result does produce consecutive tracks,
and the next largest partition has fewer tracks than our intermediate result.
Will it always break them up? Maybe not?

Say BBBC and AAA. We might build BBABACA, but

Note
----
Note, for two artists, Spotify’s algorithm works well.
But they merge in one go instead of merging pairwise,
which creates the problem.