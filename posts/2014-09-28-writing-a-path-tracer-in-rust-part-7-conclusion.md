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
To install Rust on a GNU/Linux system, all you need to do is run `rustup.sh`,
and you can get started within minutes.
On Windows it is more difficult.
First you need to install mingw-builds,
but the [wiki page][winwikipage] with instructions is a bit hidden.
Once you have that set up, the installer works fine.
You need to download [Cargo][cargo] separately.
When I started the port there was no 64-bit version yet,
but it has since been added.

[winwikipage]: https://github.com/rust-lang/rust/wiki/Using-Rust-on-Windows
[cargo]:       http://crates.io/

I have found the Rust community to be very nice.
When I did not know what to do, the IRC channel helped me out,
and the [subreddit][r/rust] was very useful as well.
The core team is around there as well,
so an answer from the experts is not uncommon.

[r/rust]: http://www.reddit.com/r/rust

At this point, Rust is a fast moving target.
During the porting process, a handful of functions has been deprecated,
some functions have been renamed,
and even some syntax has changed.
Some people describe Rust today as a different language than it was a year ago,
but I have not been using it long enough to say anything about that.
Ultimately all changes should make the language better and more consistent,
and I am confident that Rust 1.0 will be a great language.

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
