---
title: Writing a path tracer in Rust, part 2: first impressions
date: 2014-08-13 07:30
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
After porting a few files, these are my first impressions of Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Ecosystem
---------
Installing Rust and [Cargo][cargo] was easier than I expected.
With the installer, it even works on Windows without having to go through all the MSYS hassle.
The Windows version is 32-bit though.

Cargo is awesome!
It is similar to what [Cabal][cabal] is for Haskell.
Setting up a project with Cargo is as easy as writing a four-line [toml][toml] file.
Then you just do `cargo run`, and it compiles everything, and then runs the program.
The compiler produces mostly helpful error messages, telling you not only what the problem is, but also how to fix it.
Compiling and running is fast (at this point, at least).
For the few source files I have, it takes 0.46 seconds to compile and run on Linux.
That feels like compilation is _instant_.
Windows is slower, at 1.16 seconds.

<!--more-->

One downside of Cargo is that it only looks for `Cargo.toml`,
and I dislike having uppercase characters in my filenames.
At least `make` accepts `makefile` as well.
Apparently, there is [not going to be][issue45] support for `cargo.toml` either.

[cargo]:   http://crates.io
[cabal]:   http://www.haskell.org/cabal/
[toml]:    https://github.com/toml-lang/toml
[issue45]: https://github.com/rust-lang/cargo/issues/45

Style
-----
The official Rust style is to use [Egyptian brackets][egypt].
Though I prefer balanced brackets,
it is a matter of taste.
The official casing rules are Pascal casing for types, and lowercase with underscores (snake case) for most other things.
Again, this is a matter of preference, but I do find it an odd combination.
It leads to problems when a type is part of a function name, or when you name modules after a type.
The standard library itself has `TreeMap` in `treemap.rs`, but `PriorityQueue` in `priority_queue.rs`.
I chose to use snake case for my filenames.

An other thing that surprised me, is that mathematical functions use method call syntax.
That is, `x.sin()` instead of `sin(x)`.
It felt awkward at first, but I got used to it very soon.
It might even be better when multiple functions are nested, because the parentheses do not pile up.

[egypt]: http://blog.codinghorror.com/new-programming-jargon/

Modules
-------
Rust uses modules, which are like namespaces.
The compiler compiles only one file, and it might look for other files when modules are declared.
For example, I have a file `src/vector3.rs`, which will become the `vector3` module.
In `main.rs`, you declare `mod vector3;`, and that will expand to `mod vector3 { content }`, with the contents of `vector3.rs`.
This is very much like `#include` in C, and it surprised me at first.
The `Vector3` type in `vector3.rs` is used in many other modules such as `ray`, so at first I thought I should also declare `mod vector3;` in `ray.rs`.
However, that declares the module `::ray::vector3`.
The proper thing to do, is to declare both the `vector3` and the `ray` module in `main.rs`,
and then the module `::vector3` is available in `ray.rs`.
If you keep in mind that module declarations work in this `#include` kind of way, it makes sense.

Being used to the C# system, where all files are considered for name resolution,
it does feel like a step backwards.
I do not want to declare `vector3` in `main.rs`: it is a dependency of most other modules, but main does not use it directly.
Changing things in `main.rs` changes the behaviour of `ray.rs`,
even though main should depend on ray,
not the other way around.
Files interact in a [complex][complex] way.
I might have missed something though, so please let me know if there is a better way.

**Edit:** Thanks for the feedback, it changed my view.
The `use mod::Type` syntax _is_ similar to `using` directives in C#.
(Though in C# it is more common to use an entire namespace, not the individual types.)
The `mod` declarations are more like a project file, they tell the compiler which files to consider.

---

So far, translating C++ to Rust has been a pleasant experience.
When things do not work as I expected, the IRC channel is very helpful.
Next time I will discuss operator overloading with traits.

[complex]: http://www.infoq.com/presentations/Simple-Made-Easy

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/13/writing-a-path-tracer-in-rust-part-2-first-impressions
