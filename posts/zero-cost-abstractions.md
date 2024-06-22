---
title: Zero-cost abstractions
date: 2016-11-30
minutes: 6
lang: en-GB
synopsis: Rust claims to enable abstraction without overhead. How does that claim hold up in practice?
run-in: If you read my blog
teaser: neither-necessary-nor-sufficient
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
In the process I found a piece of code
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
buffer: &mut [i32];      // A mutable array slice.
coefficients: [i64; 12]; // A fixed-size array of 12 elements.
qlp_shift: i16;
```

The snippet is part of a function that restores sample values from residues.
This is something that happens a lot during decoding,
and this particular loop makes up roughly 20% of the total decoding time.
It had better be efficient.

[error-model]: /2015/06/17/exceptional-results-error-handling-in-csharp-and-rust
[claxon]:      https://github.com/ruuda/claxon

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

The only proper way to reason about the cost of these abstractions
is to inspect the generated machine code.
Claxon comes with a `decode` example program,
which I compiled in release mode with Rustc 1.13 stable.
Let’s take a look at the result:

```asm
10c00:
movslq %r14d,%r11
movslq -0x2c(%r8,%rdi,4),%rsi
imul   %r10,%rsi
movslq -0x30(%r8,%rdi,4),%r14
imul   %rbp,%r14
add    %rsi,%r14
movslq -0x28(%r8,%rdi,4),%rsi
imul   %rdx,%rsi
add    %rsi,%r14
movslq -0x24(%r8,%rdi,4),%rsi
imul   %rax,%rsi
add    %rsi,%r14
movslq -0x20(%r8,%rdi,4),%rsi
imul   %rbx,%rsi
add    %rsi,%r14
movslq -0x1c(%r8,%rdi,4),%rsi
imul   %r15,%rsi
add    %rsi,%r14
movslq -0x18(%r8,%rdi,4),%rsi
imul   %r13,%rsi
add    %rsi,%r14
movslq -0x14(%r8,%rdi,4),%rsi
imul   %r12,%rsi
add    %rsi,%r14
movslq -0x10(%r8,%rdi,4),%rsi
imul   0x8(%rsp),%rsi
add    %rsi,%r14
movslq -0xc(%r8,%rdi,4),%rsi
imul   0x18(%rsp),%rsi
add    %rsi,%r14
movslq -0x8(%r8,%rdi,4),%rsi
imul   0x20(%rsp),%rsi
add    %rsi,%r14
imul   0x10(%rsp),%r11
add    %r11,%r14
sar    %cl,%r14
add    (%r8,%rdi,4),%r14d
mov    %r14d,(%r8,%rdi,4)
inc    %rdi
cmp    %r9,%rdi
jb     10c00 <claxon::subframe::predict_lpc::h6c02f07b190820c0+0x2b0>
```

All overhead is gone **completely**.
What happened here?
First of all, no loop is used to compute the inner product.
The input slices have a fixed size of 12 elements,
and despite the use of iterators,
the compiler was able to unroll everything here.
The element-wise computations are efficient too.
There are 12 `movslq`s which load a value from the buffer and simultaneously widen it.
There are 12 multiplications and 12 additions,
11 for the inner product,
and one to add the delta
after arithmetic shifting (`sar`) the sum right.
Note that the coefficients are not even loaded inside the loop,
they are kept in registers at all times.
After the sample has been computed,
it is stored simply with a `mov`.
All bounds checks have been elided.
The final three instructions handle control flow of the loop.
Not a single instruction is redundant here,
and I could not have written this better myself.

I don’t want to end this post without at least touching briefly upon vectorisation.
It might look like the compiler missed an opportunity for vectorisation here,
but I do not think that this is the case.
For various reasons
the above snippet is not as obvious to vectorise as it might seem at first sight.
But in any case,
a missed opportunity for vectorisation is just that: a missed opportunity.
It is not abstraction overhead.

Conclusion
----------

In this post I’ve shown a small snippet of code
that uses high-level constructs such as closures and iterator combinators,
yet the code compiles down to the same instructions that a hand-written C program would compile to.
Rust lives up to its promise:
abstractions are truly zero-cost.
