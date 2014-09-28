---
title: Writing a path tracer in Rust, part 6: conclusion
date: 2014-09-28 18:42
---

As a learning exercise, I have ported the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
The result is available on [GitHub][robigo-luculenta].
In the process, I have also refreshed Luculentus a bit, updating it to modern C++.
You can about read the details in the previous posts.
In this post, I want to outline the process, and compare the final versions.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Getting started with Rust
-------------------------
- Downloading/installing, rough at first, easy now.
- IRC channel is helpful.
- Online reference is helpful.
- Use MinGW32 build, then it works.
- Fast changing language, various functions deprecated during development.

Ownership
---------
If I had to describe Rust in one word, it would be _ownership_.
In most languages, ownership is implicit.
When a function returns a pointer in C++, are you responsible for deleting it?
BLAH.
It may seem that garbage collection is a good solution to the problem.
It is not, because it only deals with memory.
Then you need an other way to free non-memory resources like a file handle,
and all ownership problems are back.
For example, the garbage collector in C# prevents use-after-free,
but there is nothing that prevents use-after-dispose.
`ObjectDisposedException` is just an access violation in disguise.

Rust _does_ prevent usage of a resource after is has been freed.
BLAH now I talk about lifetimes, not ownership.

- Single most important thing in Rust (as I see it).
- Ownership is often implicit, which means it is prone to human error.
  Rust makes it explicit, eliminating the errors.
- Forced to get right -> better design.

Updating Luculentus
-------------------
- Luculentus can benefit from ownership as well, much improvement in C++11.
- Destructors are gone.
- Count number of pointers before/after.
- Should be only one place that relies on raw pointers.


Rust vs C++
-----------
- Still don’t like Egyptian brackets. (I’ll fork `rust fmt` when it is available to fix it ;-)
- Safety not unique to Rust per se.
- Big difference: safety is opt-in, nothing guiding you to correct usage.
- Lots of material on the internet teaching the “old” C++.
- (Bjarne calls this backwards compatibility a feature …)
- Rust’s syntax is a bit more sensible.
- Compare lines of code and number of characters.
- Having `new` as a function is great, because it is not special any more.

- Immutability by default is the right thing.
- Compare occurences of `mut` and `const`, see whether there is evidence.
- No immutable (persistent) collections in the standard library though.

- Rust’s error messages are less cryptic than C++’s in most case, and usually helpful.
- Not as bad as pages of incomprehensible template expansions,
  but

    error: binary operation `/` cannot be applied to type `core::iter::Map<'_,f32,f32,core::iter::Map<'_,&[f32],f32,core::slice::Chunks<'_,f32>>>`

  is intimidating as well.

Performance
-----------
- Table here
- Revisit compilation speed (reference earlier post).

Conclusion
----------
- Both languages enable safe programming, I think, but opt-in/out.
- “Functional” things like immutable by default, pattern matching, `map`, `filter`, `Option` are nice.
- Takes more thought before code compiles, but less runtime errors (no segfaults!)
- Ownership leads to better design.
- Not written enough code to do a good comparison, but still …
- I'd choose Rust for my next project.

---

Discuss this post on [Reddit][reddit].

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/09/28/writing-a-path-tracer-in-rust-part-7-conclusion
