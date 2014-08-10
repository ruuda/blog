---
title: Writing a path tracer in Rust, part 1
date: 2014-08-10 22:30
---

It must have been almost a year ago that I first heared about [Rust][rust].
Posts about the language would appear on [/r/programming][reddit] now and then,
and the language caught my attention.
Rust got me excited for serveral reasons:

- One of the key points of Rust is a very powerful way to do deterministic resource management.
  This is a weak point of the languages that I use most often (C++ and C# nowadays).
- Rust is a systems language that offers zero-cost abstractions.
- Rust offers a very powerful type system with constructs that you would normally find in a functional language.

As a programmer who loves doing low-level optimisation,
but also appreciates high-level functional programming,
I was charmed by this new language.

[rust]:   http://rust-lang.org
[reddit]: http://reddit.com/r/programming

I followed the development for a while,
but I never got around to actually writing some code --- until now.
As an exercise, I decided to port [Luculentus][luculentus] to Rust.
Luculentus is a proof of concept spectral path tracer that I wrote for a graphics programming course.
It is written in C++.
I expect that porting it will allow me to learn many aspects of Rust.
You can follow the development of the port at [GitHub][robigo-luculenta].

I also plan on refeshing Luculentus a bit, to use more idiomatic modern C++.
When I wrote the path tracer in 2012, there was only partial support for C++11,
so Luculentus still uses a lot of raw pointers and non-default destructors.
I hope to make a fair comparison of resource management in Rust and modern C++ down the line.

[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta
