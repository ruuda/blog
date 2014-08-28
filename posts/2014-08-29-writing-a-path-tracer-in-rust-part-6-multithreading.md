---
title: Writing a path tracer in Rust, part 6: multithreading
date: 2014-08-28 21:43
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline how work is distributed among cores,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Parallelism
-----------
Luculentus is a simple path tracer.
It does not use advanced algorithms like [Metropolis light transport][mlt].
There is no [_k_-d tree][kdtree] to speed up scene intersection.
Using a better algorithm would be the best way to increase performance, but at the cost of complexity.
One way to get more performance at little extra complexity,
is to use multithreading.

[mlt]:    https://en.wikipedia.org/wiki/Metropolis_light_transport
[kdtree]: https://en.wikipedia.org/wiki/K-d_tree

Often, multithreading is _not_ a simple way to boost performance.
Synchronisation of shared state is difficult to deal with,
and even more difficult to deal with without introducing a bottleneck.
Rusts enforces thread-safety at compile time,
which eliminates an entire category of bugs that are difficult to debug.
*** meh where am I going? ***

Luckily, path tracing can be parallelised easily.
The only state that the path tracer has, is the internal buffer in the gather unit.
*** I should revisit this an other time. ***

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/29/writing-a-path-tracer-in-rust-part-6-multithreading
