---
title: Zero-cost abstractions
date: 2016-11-27
minutes: ??
synopsis: ??
run-in: ??
---

If you read my blog,
you can probably infer that I like Rust.
I like Rust for a variety of reasons.
Its [error model][error-model], for example.
But today I want to highlight a different reason:
zero-cost abstractions.

Recently I had the opportunity
to do some optimisation work on [Claxon][claxon],
my FLAC decoder.
In the process I encountered one piece of code
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
let buffer: &mut [i32];
let coefficients: [i64; 12];
let qlp_shift: i16;
```

The snippet is part of a function that restores sample values from residues.
This is something that happens a lot during decoding,
and this particular loop makes up roughly 20% of the total decoding time.
It had better be efficient then!

Ingredients
-----------

Generated code
--------------

[error-model]: /2015/06/17/exceptional-results-error-handling-in-csharp-and-rust
[claxon]:      https://github.com/ruuda/claxon
