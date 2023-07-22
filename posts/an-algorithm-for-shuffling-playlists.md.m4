---
title: An algorithm for shuffling playlists
date: 2023-07-22
minutes: 15
synopsis: When shuffling a playlist, a true shuffle is rarely what we want, because it might play the same artist twice in a row. In this post I introduce an algorithm that avoids this when possible.
run-in: It is a common observation
---

It is a common observation
that for shuffling playlists,
humans don’t want an _actual_ shuffle,
because that might play the same artist twice in a row,
which doesn’t feel random.
This topic has been discussed by others,
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

<style>
.qed { float: right }
.a { color: #c80 }
.b { color: #37b }
.c { color: #c35 }
.d { color: #690 }
.comp.a { background-color: #c80 }
.comp.b { background-color: #37b }
.comp.c { background-color: #c35 }
.comp.d { background-color: #690 }
/* The complement of a color, denoted by inverting. */
.comp {
  color: #fffef8;
  line-height: 1.4ex;
  display: inline-block;
  height: 1.8ex;
  padding-left: 0.1em;
  padding-right: 0.05em;
  border-radius: 0.1em;
  margin-right: 0.05em;
  /* Make it stand out a bit less, it already draws a lot of attention. */
  opacity: 0.8;
}
.s + .s { margin-left: 0.03em }
.s + .comp { margin-left: 0.05em }
</style>
changequote(`[[', `]]')
define([[_var]], [[<var>$1</var>]])
define([[v_b]], [[_var(b)]])
define([[v_k]], [[_var(k)]])
define([[v_n]], [[_var(n)]])
define([[v_m]], [[_var(m)]])
define([[v_r]], [[_var(r)]])
define([[v_r0]], [[_var(r)<sub>0</sub>]])
define([[v_r1]], [[_var(r)<sub>1</sub>]])
define([[v_s]], [[_var(s)]])
define([[v_x]], [[_var(x)]])
define([[v_xp]], [[_var(x')]])
define([[v_y]], [[_var(y)]])
define([[v_yp]], [[_var(y')]])
define([[seq_aa]], [[patsubst($1, [[\([A]+\)]], [[<abbr class="s a">\1</abbr>]])]])
define([[seq_bb]], [[patsubst($1, [[\([B]+\)]], [[<abbr class="s b">\1</abbr>]])]])
define([[seq_cc]], [[patsubst($1, [[\([C]+\)]], [[<abbr class="s c">\1</abbr>]])]])
define([[seq_dd]], [[patsubst($1, [[\([D]+\)]], [[<abbr class="s d">\1</abbr>]])]])
define([[seq_comp_aa]], [[patsubst($1, [[\([BCD]+\)]], [[<abbr class="comp a">\1</abbr>]])]])
define([[seq_comp_bb]], [[patsubst($1, [[\([ACD]+\)]], [[<abbr class="comp b">\1</abbr>]])]])
define([[seq_comp_cc]], [[patsubst($1, [[\([ABD]+\)]], [[<abbr class="comp c">\1</abbr>]])]])
define([[seq_comp_dd]], [[patsubst($1, [[\([ABC]+\)]], [[<abbr class="comp d">\1</abbr>]])]])
define([[_seq]], [[seq_aa(seq_bb(seq_cc(seq_dd($1))))]])
define([[_comp_a]], [[seq_aa(seq_comp_aa($1))]])
define([[_comp_b]], [[seq_bb(seq_comp_bb($1))]])
define([[_comp_c]], [[seq_cc(seq_comp_cc($1))]])
define([[_comp_d]], [[seq_dd(seq_comp_dd($1))]])
define([[aA]], [[seq_aa(A)]])
define([[aB]], [[seq_bb(B)]])
define([[aC]], [[seq_cc(C)]])
define([[aD]], [[seq_dd(D)]])

Notation
--------
In this post a playlist is an ordered list of tracks.
For example,
_seq(AAABBC) is a playlist with six tracks:
three by artist aA,
then two by artist aB,
and then one by artist aC.
The shuffling algorithm returns a permutation of this list;
it specifies the order of the artists.
At this point we treat tracks by the same artist as indistinguishable.
We’ll address that later.

Interleaving two artists
------------------------
Suppose we have tracks from just two artists, aA and aB,
where aA has more tracks than aB.
Then we can divide aA’s tracks into equally sized groups,
and insert aB’s tracks in between them.
This way we never play aB consecutively,
we break up as many of aA’s consecutive plays as we can,
and we minimize the longest span of aA’s in a row.

Let’s state that formally.
We are going to shuffle a playlist with two artists,
aA with v_n tracks,
and aB with v_m tracks:

1. Partition on artist,
   so we have list v_x of length v_n
   and list v_y of length v_m,
   with v_n ≥ v_m.
2. Split v_x into v_m + 1 equal parts.
   If v_m + 1 does not divide v_n,
   the parts that get one more element can be selected randomly.
4. Interleave the v_m + 1 parts of v_x with v_y’s elements.
   If v_m = v_n,
   we just interleave the two lists
   (we can split v_x into at most v_m parts),
   but we can flip a coin about which list goes first.

The resulting permutation is optimal in the following sense:

 * Artist aB will have no consecutive tracks,
   and if possible aA will not either.
 * More generally, for any positive integer v_k,
   the number of occurrences of v_k consecutive tracks
   by the same artist is minimal.

We can apply this interleaving more generally,
we don’t need to limit ourselves to inputs v_x and v_y
that consist of only one artist each.
The procedure can interleave any two lists v_x and v_y.
We will need this later,
so let’s call the procedure `interleave`.
It interleaves the smaller list into the larger one.

Multiple artists
----------------
Now that we can shuffle two artists,
we can extend the idea to more artists.
Let’s look at an example first.
Say we have four tracks by artist aA,
two by aB, and one by aC.
Then the three optimal shuffles are _seq(ABABACA), _seq(ABACABA), and _seq(ACABABA).
Among aB and aC we have some freedom,
but we need all the aB’s and aC’s
together to go in between the aA’s.
What we did is to first interleave aB and aC,
and then we interleaved the result with aA.

A promising approach then,
is to incrementally build up the result from an empty list.
At every step,
we interleave an artist with the least number of tracks with the result,
until we incorporated all artists.
This works well in most cases,
and I originally thought that this algorithm would produce optimal shuffles.
However,
when I set out to prove that,
I discovered a counterexample.

Take 2 tracks of artist aA,
4 tracks of aB,
and 4 tracks of aC.
If we interleave aA and aB first,
then aA partitions the aB’s into three groups,
two of size one,
and one of size two.
For example _seq(BBABAB).
This list has length 6,
so next we interleave the aC’s into it,
but the four aC’s are not quite enough to only create groups of size 1,
there will be one group of size 2,
and it might contain a _seq(BB).
We _do_ have enough aC’s to break up all the occurences of _seq(BB),
but we need to be more careful about where we use them.
We can fix the counterexample by interleaving aB and aC first,
and then interleaving aA into it.
But it turns out that there exist counterexamples
that cannot be fixed by interleaving in a different order.
For example, 4, 8, and 10 tracks.
No matter which two artists we interleave first,
it is not possible for `interleave` to guarantee an optimal shuffle.

To tackle the counterexample we can define `intersperse`
which merges two lists v_x and v_y of length v_n and v_m.
It proceeds as follows:

 1. Break v_x up into spans at every place where an artist plays twice.
 2. While we have fewer than v_m + 1 spans,
    break up spans randomly until we have v_m + 1.
 3. Interleave the spans with elements of v_y.

Note that in general,
step 1 may produce more than v_m + 1 spans,
and then the procedure doesn’t work.
But we are only going to use `intersperse`
in situations where this does not happen.

Incremental merging for optimal shuffles
----------------------------------------
Now we can put the pieces together
and fix the issue that plagued our first attempt at incremental interleaving.
Define the `merge-shuffle` procedure as follows:

 1. Partition on artist, and order the partitions by ascending size.
    Break ties randomly.
 2. Initialize v_r to an empty list.
 3. Take the next partition and merge it into v_r.
    Say the partition to merge has length v_n,
    and v_r has length v_m.
    When v_n ≥ v_m,
    use `interleave`.
    When v_n < v_m,
    use `intersperse`.
 4. Repeat until we merged all partitions, v_r is the result.

Why is `intersperse` safe to use when v_n < v_m?
This happens because v_r can have at most v_n consecutive tracks,
and that property holds at every step of the procedure.
The proof below makes it a bit clearer why this happens,
but for the purpose of implementing the algorithm,
we can take it as a fact that `intersperse` will not fail.

This algorithm produces optimal shuffles,
which we will formalize later.
The only case where it places the same artist consecutively
is when this is impossible to avoid,
because we don’t have enough other tracks to break up the consecutive plays.
This happens when the last step is an `interleave` and v_n > v_m + 1,
for example in _seq(AABA).

Extension to albums
-------------------
In a situation like _seq(AABA),
we cannot avoid playing artist aA multiple times in a row.
But if two of aA’s tracks are from a different album,
then we can at least avoid playing tracks from the same album in a row.
The process is the same as before,
but instead of partitioning on artist we partition on album.
The merge-shuffle preserves the relative order
of tracks in the partitions that it merges,
so to shuffle a playlist,
we can use `merge-shuffle` at two levels:

 1. Partition the playlist into albums.
 2. Shuffle every album individually using a regular shuffle.
 3. For every album artist,
    use `merge-shuffle` to merge albums into a partition per artist.
 4. Use `merge-shuffle` to merge the artist partitions
    into a final shuffled playlist.

Optimality proof
-----------------
Let’s first formalize what we are trying to prove.

**Definition**:
For a positive integer v_k,
the <em>v_k-badness</em> of a playlist
is the number of times that
the same artist occurs v_k times in a row.
For example, the 2-badness of _seq(AAABBC) is 3:
_seq(AA) occurs at index 0 and 1,
and _seq(BB) occurs at index 3.

**Definition**:
Let v_x and v_y be permutations of the same playlist.
We say that v_x is _better_ than v_y if both of the following hold:

 1. There exists a positive integer v_k
    for which v_x has a lower v_k-badness than v_y.
 2. For every positive integer v_k,
    v_x has no greater v_k-badness than v_y.

This defines a partial order on playlists.
Note that this is not a total order!
For example,
_seq(AAABAABAABAAB) has a lower 3-badness than
_seq(AAABAAABABABA), but a higher 2-badness.

**Definition**:
A permutation of a playlist is called an _optimal shuffle_
if there exists no better permutation.
For example,
_seq(AAABB) is not an optimal shuffle,
because its permutation _seq(ABBAA) has a 2-badness of 2,
lower than _seq(AAABB)’s 2-badness of 3,
and it also has a lower 3-badness.
An example of a shuffle that _is_ optimal is _seq(BBCB).
It has a 2-badness of 1,
and of its four permutations (_seq(CBBB), _seq(BCBB), _seq(BBCB), and _seq(BBBC)),
none achieve a lower 2-badness.

**Theorem**:
The `merge-shuffle` algorithm returns optimal shuffles.
Moreover, the 2-badness of the result is at most v_n - 1,
where v_n is the track count for the artist that occurs most often.<br>
_Proof_:
The proof will be by induction on v_s, the number of artists.

**Base case**:
With a single artist,
there is nothing to interleave or intersperse,
and the result has a 2-badness of v_n - 1,
where v_n is the size of the input.

**Induction step**:
Let the number of artists v_s be given.
Assume that for fewer than v_s artists,
our algorithm produces optimal shuffles.
Say we have v_n tracks by aA,
and v_m tracks by different artists (not necessarily all distinct).
Assume that no other artist occurs more than v_n times.
Let v_x be an optimal shuffle of the v_m tracks
with a 2-badness of at most v_n - 1.
We can distinguish three cases:

1. When v_n > v_m,
   we interleave the tracks from v_x between the aA’s.
   We break up the list of aA’s into spans of size
   v_k = ⌈v_n / (v_m + 1)⌉ and possibly of size v_k - 1.
   It is not possible to build a permutation with lower v_k-badness
   without breaking the list into more than v_m + 1 spans.
   This also holds for the 2-badness, 3-badness, ... up to v_k,
   so the permutation is optimal.
   Moreover, the 2-badness is less than v_n - 1:
   a list of all aA’s would have 2-badness v_n - 1,
   and interleaving tracks from v_x can only reduce this further.
2. When v_n = v_m
   we can interleave aA’s with tracks from v_x.
   The result has a v_k-badness of zero for all v_k ≥ 2,
   which is optimal.
3. When v_n < v_m,
   we use `intersperse`.
   The aA’s are used in between spans,
   so there is no badness due to aA.
   The 2-badness of v_x is at most v_n - 1,
   but we have v_n aA’s,
   which is enough to eliminate all 2-badness entirely.
   Therefore the resulting shuffle is optimal.

This concludes the proof. <span class="qed">∎</span>

Completeness
------------
We now have an algorithm that generates optimal shuffles,
for some very specific definition of optimal.
We might call this _soundness_:
the algorithm never generates shuffles that are not optimal.
However,
we haven’t shown what we might call _completeness_ or _surjectivity_:
for a given playlist,
can the algorithm output every possible optimal shuffle?
And if the answer is yes,
do each of the shuffles have equal probability of being generated,
or is the algorithm biased in some way?

It turns out that `merge-shuffle` is not complete.
For example,
for the input _seq(AAABBC)
it would only output _seq(ABACAB) or its mirror image _seq(BACABA),
but not _seq(ACABAB),
even though all of them are optimal under the current definition.

<!--
We could argue that _seq(ABACAB)
is a nicer playlist than _seq(ACABAB) anyway,
because the artists are distributed more evenly.
This is not so obvious in this small example,
but we can scale it to arbitrarily large playlists,
where the first half would consist of _seq(AB) repeated many times,
and the second half of _seq(AC).
That would be optimal under the current definition,
but does it match our expectations of a good shuffle?
On the one hand I like the evenness better for playlists,
so maybe we should require this of an optimal shuffle.
But on the other hand,
it constrains the number of possible shuffles severely,
which arguably defeats the point of shuffling.
-->

More even shuffles
------------------
An arguably desirable property of shuffles,
is to have an artist’s tracks occur roughly uniformly throughout the playlist.
The current definition of optimal
penalizes consecutive tracks by the same artist,
but as long as that does not happen,
it doesn’t care where in the playlist tracks occur.
For instance,
_seq(ABABCDCD) and _seq(ABCDABCD) are both optimal,
even though artists are clustered in the former
and spaced more evenly in the latter.
To capture this,
we might extend the definition of badness to negative integers,
where the (–v_k)-badness of a playlist
is the number of times that the _complement_ of an artist
occurs v_k times in a row.

Let’s look at an example.
Consider aA in _seq(ABABCDCD).
We can highlight its complement as _comp_a(ABABCDCD),
the complement of aB as _comp_b(ABABCDCD), etc.
These two complements have a 2-badness of 4 and 3 respectively,
and by symmetry we also get 4 and 3 from the complements
_comp_c(ABABCDCD) and _comp_d(ABABCDCD) of aC and aD.
Therefore the (–2)-badness of _seq(ABABCDCD) is 14.
Through similar reasoning we can count that the (–3)-badness is 10,
the (–4)-badness is 6,
and the (–5)-badness is 2.

Now let’s look at _seq(ABCDABCD).
Here the complement of aA is _comp_a(ABCDABCD),
the complement of aB is _comp_b(ABCDABCD),
the complement of aC is _comp_c(ABCDABCD),
and the complement of aD is _comp_d(ABCDABCD).
The (–2)-badness again comes out to 14,
but the (–3)-badness is reduced to 6,
and (–4)-badness does not occur at all.
If we extend our definition of _better_ and _optimal_ permutations
to include negative values of v_k,
then _seq(ABCDABCD) would be a better permutation than _seq(ABABCDCD).

Even though we saw already that our merge-shuffle
avoids generating some uneven shuffles,
it is by no means sound
under this new extended definition of optimal.
For instance,
it would output permutations worse than _seq(ABCDABCD).
To design an algorithm
that generates optimal shuffles for the extended definition of optimal
would be an interesting topic for future work.
Especially if the algorithm can be made complete and unbiased,
in the sense that it outputs all possible optimal shuffles
with equal probability.

In practice
-----------
[I implemented the merge-shuffle algorithm in Musium][impl].
It works reasonably well in practice,
although I find the shuffles are sometimes lacking variation
when I re-shuffle.
This is no surprise because for many inputs,
the order of the artists is deterministic.
There are ways to alleviate this by sacrificing some evenness,
for instance by allowing to split the larger list
into only v_m parts rather than v_m + 1
when the smaller list has v_m tracks.
I also wonder if it would help to always merge the two smallest lists
instead of always merging into the intermediate result.
Perhaps the result would no longer be optimal
according to the definition in this post,
but although this definition is good for nerd-sniping,
it is not so obvious that it captures what humans care about well.
Arriving at a good shuffling algorithm will require some more experimentation.

Aside from the algorithm itself,
[I implemented a fuzzer][fuzz] that searches for counterexamples
to the proof in this post.
After implementing the full algorithm outlined in this post,
the fuzzer has not found any counterexamples.
The value of this is twofold:
it increases my confidence that the implementation is correct,
but it also increases my confidence that the proof is correct.

Performance-wise the algorithm is fast enough to be a non-issue.
Although the complexity in the number of track moves is worst-case quadratic
(achieved when all artists are distinct,
because we build v_n lists of size v_n/2 on average)
in practice shuffling even thousands of tracks takes mere microseconds,
so unless you try to shuffle your entire music library,
this is unlikely to be a problem.

[impl]: https://github.com/ruuda/musium/blob/e4939b6031c477d5d7cd2c5d520e9337edd8a383/src/shuffle.rs
[fuzz]: https://codeberg.org/ruuda/musium/src/commit/e4939b6031c477d5d7cd2c5d520e9337edd8a383/fuzz/fuzz_targets/shuffle.rs

Conclusion
----------
In this post I gave a new algorithm for shuffling playlists.
This algorithm avoids playing the same artist twice in a row
when this is possible to avoid,
unlike other algorithms such as the one introduced by Spotify.
I also proved this,
which was a valuable exercise,
because it uncovered a flaw in an earlier version of the algorithm.
The further I got writing this post though,
the clearer it became to me how ad-hoc the algorithm really is,
and the more I used the shuffle myself,
the less clear it became to me
that the current definition of optimal is a good one.
I could keep on refining things for a long time,
but then I would never publish anything,
so this is where I draw the line for now.
The merge-shuffle works well enough in practice.
Maybe I’ll revisit it some other time.
