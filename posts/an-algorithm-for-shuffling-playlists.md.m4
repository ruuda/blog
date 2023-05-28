---
title: An algorithm for shuffling playlists
date: 2023-05-27
minutes: ?
synopsis: TODO
run-in: It is widely known
---
define(`_abbr', `<abbr>$1</abbr>')
define(`_var', `<var>$1</var>')
define(`aA', `_abbr(A)')
define(`aB', `_abbr(B)')
define(`aC', `_abbr(C)')
define(`v_k', `_var(k)')
define(`v_n', `_var(n)')
define(`v_m', `_var(m)')

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
Suppose we have a playlist with just two artists, aA and aB,
and they have v_n and v_m tracks respectively.
Without loss of generality we may assume that v_n ≥ v_m.
We can create a playlist without consecutive tracks by the same artist
if and only if v_m ≥ v_n - 1.
With v_m = v_n - 1
we can put one of aB’s track in between each of aA’s tracks.
If we had any less,
then some of aA’s tracks will need to be consecutive.
With v_m = v_n we can also add a track at the start or end,
and v_m > v_n would violate our assumption.

From this it is not so hard to see
how we can optimally shuffle a playlist with two artists:

1. Partition on artist,
   so we have list aA of length v_n
   and list aB of length v_m,
   with v_n ≥ v_m.
2. Shuffle the lists individually using a true shuffle.
   (Later we will refine this
   to at least try and avoid consecutive tracks from the same album.)
3. Split list aA into v_m + 1 equal parts.
   If v_m + 1 does not divide v_n,
   the parts that get one more element can be decided pseudorandomly.
4. Interleave the v_m + 1 parts of aA with aB’s elements.

This algorithm is optimal in the following sense:

 * Artist aB will have no consecutive tracks,
   and if possible aA will not either.
 * More generally, for any positive integer v_k,
   the number of occurrences of v_k or less consecutive tracks
   by the same artist is minimal.

Multiple artists
----------------
Now that we can optimally shuffle two artists,
we can extend the idea to more artists.
Let’s look at an example first.
Say we have four tracks by artist aA,
two by aB, and one by aC.
Then the three optimal shuffles are ABABACA, ABACABA, and ACABABA.
Between aB and aC we have some freedom,
but we need all the aB’s and aC’s
together to go in between the aA’s.
What we did here
is to first interleave aB and aC,
and then we interleaved the result with aA.

If we have an already shuffled list of v_n tracks
and we want to merge in v_m more tracks by a new artist not in the list,
then if v_n ≥ v_m,
we can do this without creating consecutive tracks by the same artist.
In other words,
when we consider a new artist with v_m tracks,
we should try to have an already-shuffled list of at least v_m - 1 tracks.

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