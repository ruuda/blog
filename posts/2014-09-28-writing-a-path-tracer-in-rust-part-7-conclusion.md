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
For me, this is the one thing that sets Rust apart from other languages.
In most languages, ownership is implicit,
and this leads to several kinds of errors.
When a function returns a pointer in C++, who is responsible for deleting it?
Can you answer that question without consulting the documentation?
And even if you know the answer, it is still possible to forget a delete,
or accidentally delete twice.

This problem is not specific to pointers though, it is a problem with resources in general.
It may seem that garbage collection is a good solution,
but it is not, because it only deals with memory.
Then you need an other way to free non-memory resources like a file handle,
and all ownership problems are back.
For example, the garbage collector in C# prevents use after free,
but there is nothing that prevents use after _dispose_.
Is an `ObjectDisposedException` that much better than an access violation?

Use after free is realy a problem with _lifetimes_:
an object or pointer is still around,
while the resource has already been freed.
[RAII][raii] solves this problem,
but this only works if you go all the way.
If you have a raw pointer to an RAII object in C++, you can still forget to delete it.

HOW DO I UNIFY LIFETIMES AND OWNERSHIP?

[raii]: https://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization

Rust _does_ prevent usage of a resource after is has been freed.
BLAH now I talk about lifetimes, not ownership.

- Single most important thing in Rust (as I see it).
- Ownership is often implicit, which means it is prone to human error.
  Rust makes it explicit, eliminating the errors.
- Forced to get right -> better design.

Updating Luculentus
-------------------
The benefits of RAII are not specific to Rust.
It is perfectly possible to write similar code in modern C++,
which is arguably a very different language than pre-2011 C++.
When I wrote Luculentus, C++11 was only partially supported.
There were lots of raw pointers that are nowadays not necessary.
I have replaced most raw pointers in Luculentus with `shared_ptr` or `unique_ptr`,
and arrays with vectors.
As a consequence, _all_ manual destructors are now gone.
(There were six previously.)
Before, there were eleven delete statements.
Now there are zero.
All memory management has become automatic.
Apart from the arguments to main, there is only one place left that uses raw pointers.

Porting the path tracer to Rust also improved its design.
If your resource management is wrong, it is invalid in Rust.
In C++ you can get away with e.g. dereferencing the first element of a vector,
and when the vector goes out of scope, the pointer will be invalid.
The code is valid C++ though.
Rust does not allow shortcuts like that,
and for me it has opened my eyes to an area that I was not fully aware of before.
Even when working in other languages,
if a construct would be illegal in Rust,
there probably is a better way.

This demonstrates that it _is_ possible to write safe code in C++.
You _do_ get safe, automatic memory management with virtually no overhead.
The only caveat is that you must choose to leverage it.
You could use a `unique_ptr`, but you could just as well use a raw pointer.
All the dangerous tools of the ‘old’ C++ are still there,
and you can mix them with modern C++ if you like.
Of course there is value in having old code compile (Bjarne calls it a [feature][feature]),
but I would prefer to not implicitly mix two quite different paradigms,
and keep all the past design mistakes around.
It takes some time to unlearn using `new` and `delete`,
and even then, old APIs will be around for a long time.

[feature]: http://channel9.msdn.com/Events/GoingNative/2013/Opening-Keynote-Bjarne-Stroustrup

- Luculentus can benefit from ownership as well, much improvement in C++11.
- Destructors are gone.
- Count number of pointers before/after.
- Should be only one place that relies on raw pointers.

In the end, I am convinced that it is possible to write safe code in C++ as well as in Rust.
You have to be more careful in C++,
and the compiler does not guide you as much,
but it can be done.

Rust vs C++
-----------
A nice thing about Rust is that it can start from scratch,
and learn from the mistakes of earlier languages.
C++11 is a lot better than the version before it,
but it only _adds_ features,
and every new feature cannot break old code.
One point where this shows is syntax.
In Rust, types go after the name, and a return type comes after the argument list,
which is the sensible thing to do.
Rust’s lambda syntax is more concise, and there is less repetition.
(I still cannot get used to the Egyption brackets though.
They look **wrong** to me.)

Another area where I think Rust made the right choice, is mutability.
In Rust, everything is immutable by default, whereas in C++ everything is mutable by default.
The Luculentus codebase has 535 occurences of `const` at the moment of writing.
Robigo Luculenta has only 97 occurences of `mut`.
Of course there is more duplication in C++,
but this still suggests that immutable is a more sensible default.
The Rust compiler warns about variables that need not be mutable, which is nice.

If I compare the number of non-whitespace source characters,
the C++ version has 108981 characters — excluding the CIE1964 files that I did not port,
whereas the Rust version has 73603 characters,
only two thirds the size of the C++ version.

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
I added basic performance counters to Luculentus and Robigo Luculenta.
It counts the number of trace tasks completed per second.
These are the results:

Compiler               Platform        Performance
---------------------  --------------  -----------
GCC 4.9.1              Arch Linux x64  0.33 ± 0.06
GCC 4.9.1*             Arch Linux x64  0.35 ± 0.04
Clang 3.5.0            Arch Linux x64  0.30 ± 0.05
msvc 110               Windows 7 x64   0.23 ± 0.03
msvc 110*              Windows 7 x64   0.23 ± 0.02
rustc 0.12 2014-09-23  Windows 7 x64   0.23 ± 0.01
rustc 0.12 2014-09-25  Arch Linux x64  0.32 ± 0.01

Optimisation levels were set as high as possible everywhere.
The compilers with asterisk used profile-guided optimisation.
The only conclusion I can draw from this,
is that you should probably not use Windows if you want performance.

In my first post I noted that rustc compiles extremely fast,
but there was very little code at that point.
After the port, these are the compile times in seconds:

Compiler               Time
---------------------  ------------
GCC 4.9.1              17.3  ± 0.5
Clang 3.5.0            13.39 ± 0.03
msvc 110               20.4  ± 0.3
rustc 0.12 2014-09-26   7.31 ± 0.05

No instant compilation any more, but still much better than C++.

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
