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
It does not use advanced algorithms like [Metropolis light transport][mlt],
and tere is no [_k_-d tree][kdtree] to speed up scene intersection.
Using better algorithms would be the best way to increase performance, at the cost of complexity.
One way to get more performance with little extra complexity,
is to just throw more computing power at the problem.

[mlt]:    https://en.wikipedia.org/wiki/Metropolis_light_transport
[kdtree]: https://en.wikipedia.org/wiki/K-d_tree

The path tracing process so far has been pretty straightforward:
generate some random rays,
determine their contribution to the final image,
mix the contribution with earlier contributions,
and convert that to an image that a monitor can display.
The first three steps are performed in a loop,
and once in a while the fourth step is performed to visualise the current render state.

This process can be parallelised without much synchronisation.
Threads can do the loops in parallel.
Only access to the buffer in which all contributions are accumulated, needs to be synchronised.
Something also needs to ensure that an image is generated periodically.

<!--more-->

The task scheduler
------------------
The task scheduler determines what threads should do next when they run out of work.

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/29/writing-a-path-tracer-in-rust-part-6-multithreading
