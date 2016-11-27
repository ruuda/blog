---
title: Zero-cost abstractions
date: 2016-11-27
minutes: ??
synopsis: ??
run-in: If you read my blog
---

If you read my blog,
you can probably guess that I like Rust.
I like Rust for a variety of reasons.
Its [error model][error-model], for example.
But today I want to highlight a different reason:
zero-cost abstractions.

Recently I had the opportunity
to do some optimisation work on [Claxon][claxon],
my FLAC decoder.
In the process I encountered a piece of code
which in my opinion demonstrates the power of Rust very well,
and I would like to share that with you here:

```rust
for i in 12..buffer.len() {
    let prediction = coefficients.iter()
                                 .zip(&buffer[i - 12..i])
                                 .map(|(&c, &s)| c * s as i64)
                                 .sum::<i64>() >> qlp_shift;
    let delta = buffer[i];
    buffer[i] = prediction as i32 + delta;
}
```

For context, the following variables are in scope:

```rust
buffer: &mut [i32];
coefficients: [i64; 12];
qlp_shift: i16;
```

The snippet is part of a function that restores sample values from residues.
This is something that happens a lot during decoding,
and this particular loop makes up roughly 20% of the total decoding time.
It had better be efficient then.

Ingredients
-----------

The snippet computes a fixed-point arithmetic inner product
between the coefficients and a window that slides over the buffer.
This value is the prediction for a sample.
After adding the residue to the prediction, the window is advanced.
An inner product can be neatly expressed by zipping the coefficients with the window,
mapping multiplication over the pairs,
and then taking the sum.
I would call this an abstraction:
it moves the emphasis away from how the value is computed,
focusing on a declarative specification instead.

The snippet is short and clear -- in my opinion --
but there is quite a bit going on behind the scenes.
Let’s break it down.

 * `12..buffer.len()` constructs a `Range` structure with an upper and lower bound.
   Iterating over it with a for loop implicitly creates an iterator,
   however in the case of `Range` that iterator is the structure itself.
 * `coefficients().iter()` constructs a slice iterator,
   and the call to `zip` implicitly constructs an iterator for the `buffer` slice as well.
 * `zip` and `map` both wrap their input iterators in a new iterator structure.
 * A closure is being passed to `map`.
   The closure does not capture anything from its environment in this case.
 * `sum` repeatedly calls `next()` on its input iterator,
   pattern matches on the result,
   and adds up the values until the iterator is exhausted.
 * Indexing into and slicing `buffer` will panic if an index is out of bounds,
   as Rust does not allow reading past the end of an array.

It appears that these high-level constructs come at a price.
Many intermediate structures are created
which would not be present in a hand-written inner product.
Fortunately these structures are not allocated on the heap,
as they would likely be in a language like Java or Python.
Iterators also introduce extra control flow:
`zip` will terminate after one of its inner iterators is exhausted,
so in principle it has to branch twice on every iteration.
And of course iterating itself involves a call to `next()` on every iteration.
Are we trading performance for convenience here?

Generated code
--------------

[error-model]: /2015/06/17/exceptional-results-error-handling-in-csharp-and-rust
[claxon]:      https://github.com/ruuda/claxon
