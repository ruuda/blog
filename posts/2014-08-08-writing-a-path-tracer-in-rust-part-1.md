---
title: Writing a path tracer in Rust, part 1
date: 2014-08-08 21:19
---

I do not remember first hearing about Rust.
It must have been almost a year ago.
Posts about Rust would appear on [/r/programming][reddit] now and then,
and I started to follow the development of the language more closely.
Rust caught my attention for several reasons.
I think convenient safe and deterministic resource management is a major problem in the languages I use most often (C++ and C# nowadays),
and this is one of the key points of Rust.
Rust offers a very rich type system like the functional languages.
And Rust is a systems language, providing zero-cost abstractions.
As a programmer who loves doing low-level optimisation,
but also appreciates high-level functional constructs,
I was charmed by Rust.

[reddit]: http://reddit.com/r/programming

I followed the development for a while,
but I never got around to actually writing some code.
Until now.
As an exercise, I decided to port [Luculentus][luculentus] to Rust.
Luculentus is a proof of concept spectral path tracer that I wrote in C++ for a graphics programming course.
I think it is a nice program to get to know Rust:
Blah.
You can follow the development of the port at [GitHub][robigo-luculenta].
I also plan on refeshing Luculentus a bit, to use more idiomatic modern C++.
When I wrote the path tracer in 2012, there was only partial support for C++11,
so Luculentus still uses a lot of raw pointers and non-default destructors.
I hope to make a fair comparison between modern C++ and Rust down the line.

[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta
