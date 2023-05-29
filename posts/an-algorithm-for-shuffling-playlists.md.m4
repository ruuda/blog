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
define(`v_s', `_var(s)')
define(`v_x', `_var(x)')
define(`v_y', `_var(y)')
define(`k_0', `_var(k)<sub>0</sub>')
define(`k_1', `_var(k)<sub>1</sub>')

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
In this post I want to outline an algorithm that is optimal in the above sense.

[martin]: https://keyj.emphy.de/balanced-shuffle/
[spotify]: https://engineering.atspotify.com/2014/02/how-to-shuffle-songs/
[musium]: https://github.com/ruuda/musium

Interleaving two artists
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
   the parts that get one more element can be selected randomly.
4. Interleave the v_m + 1 parts of aA with aB’s elements.
   If v_m = v_n,
   we just interleave the two lists
   (we can split aA into at most v_m parts),
   but we can flip a coin about which list goes first.

This algorithm is optimal in the following sense:

 * Artist aB will have no consecutive tracks,
   and if possible aA will not either.
 * More generally, for any positive integer v_k,
   the number of occurrences of v_k consecutive tracks
   by the same artist is minimal.

Multiple artists
----------------
Now that we can shuffle two artists,
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
   interleave the smallest partition with the intermediate result,
   as in step 3 and 4 of the previous section.
   If there is no unique smallest partition,
   we can break ties randomly.

The intuition behind this is that
by interleaving the smallest partitions first,
we ensure that by the time we get to a larger partition,
we have enough tracks to interleave it with.
If at some point we produced consecutive tracks in the intermediate list,
then interleaving it into a larger partition
will break up the consecutive tracks.

Could it happen that we end up with consecutive tracks in the intermediate list,
— let’s say BBCB —
but the next partition is smaller than the intermediate list
— let’s say AA —
so it gets interleaved into the intermediate list
instead of the other way around,
and not all consecutive tracks get broken up?
It turns out this cannot happen,
but this is not so obvious,
we will prove it in the next section.
In the example,
we should have interleaved AA and aC first because those are the smaller partitions,
and then interleaving with BBB works out fine.

Appendix: Optimality proof
--------------------------
Let’s first formalize what we are trying to prove.
To make the proof a bit cleaner,
let’s forget about artists and tracks for a moment,
and consider lists of _symbols_ instead,
such as AAABBC where in this example the symbols are aA, aB, and aC.

**Definition**:
For a positive integer v_k,
the <em>v_k-badness</em> of a list of symbols
is the number of times that
the same symbol occurs v_k times in a row.
For example, the 2-badness of AAABBC is 3:
AA occurs at index 0 and 1,
and BB occurs at index 3.

**Definition**:
If a permutation v_x has a lower v_k-badness than v_y,
then v_x also has a lower (v_k + 1)-badness than v_y,
because every sequence of v_k + 1 times the same symbol
has two subsequences of v_k times the same symbol.
This means that we can speak about one pemutation being better
than another regardless of v_k.
We say that a permutation v_x is _better_ than v_y,
if there exists a positive integer v_k
such that v_x has a lower v_k-badness than v_y.
This defines a total order on the permutations of v_x.

**Definition**:
A permutation v_x of a list of symbols is called an _optimal shuffle_,
if there exists no better permutation.
For example,
AAABBC is not an optimal shuffle,
because its permutation ABBACA has a 2-badness of 2,
lower than AAABBC’s 2-badness of 3.
An example of a shuffle that _is_ optimal is AABA.
It has a 2-badness of 1,
and of its four permutations (BAAA, ABAA, AABA, and AAAB),
none achieve a lower 2-badness.

**Theorem**:
The algorithm described in the previous section returns optimal shuffles.<br>
_Proof_:
The proof will be by induction on the number of symbols v_s.

**i**. For v_s = 2,
say we have v_n times the symbol aA,
and v_m ≤ v_n times the symbol aB.
When v_m = v_n we interleave the two lists
and the result has a v_k-badness of zero for every v_k ≥ 2,
which is optimal.
Assume therefore that v_m < v_n.
Then we break up the list of aA’s into groups of size
v_k = ⌈v_n / (v_m + 1)⌉ and possibly of size v_k - 1.
This permutation has the minimal v_k-badness,
and reducing the badness for shorter spans would create more v_k-badness,
so the permutation is optimal.

Note
----
Note, for two artists, Spotify’s algorithm works well.
But they merge in one go instead of merging pairwise,
which creates the problem.