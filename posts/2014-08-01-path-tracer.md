---
title: Writing a path tracer in Rust, part 1
date: 2014-08-01 21:00
---

- Papercuts and major design flaws in languages like C++ and C#.
- Rust appears to solve the flaws that I find most important.
x Wanted to try Rust for a very long time.
x Porting Luculentus to Rust.
x Also updating Luculentus codebase from 2012 (when C++11 was not widely available) to use idiomatic C++11.
- Destructors in C++ code should be gone.
- Conclusion? Performance comparison?

Porting specifics:
x Cargo is really, really nice. Easy to set up, and then just `cargo run` after writing some code.
x The compiler has mostly helpful error messages. Lots of warnings, which is good as well.
x I still find the Egyptian brackets a bit awkward, but it is mostly a matter of preference, and I think I will get used to it.
x It seems to be impossible to have multiple implementations of `Mul<T, U>`. I encountered this when trying to allow scalar multiplication for vectors on both sides.
  With some help from IRC (they were really quick), I found a workaround, but it does not solve the problem in all cases.
  Note that this essentially requires overloading based on traits, if it would be solved in a generic manner.
x The module structure and file layout can be a bit confusing. I had `vector3.rs` and `quaternion.rs`.
  Then `quaternion.rs` requires `vector3.rs`. To resolve, I had to add use statements to `main.rs`.
  Being used to C# this does not really make sense, but if you treat `mod` declarations as C++ `#includes`,
  and if you are aware that `main.rs` is the only file that matters to the compiler,
  then it does make sense.
x Having to use method call syntax for mathematical functions feels weird. Is there an other way?
x I _really_ dislike the Egyptian brackets! They make everything look unbalanced. :(
x If structs use Pascal casing, but files use lower case, how should I name the file and mod for `MonteCarloUnit`?
  The standard library itself is inconsistent. Libcollections has `TreeMap` in `treemap.rs`, but `PriorityQueue` in `priority_queue.rs`.
  I will use snake case for filenames and module names then, it seems nicer.
  (The casing conventions in Rust are a bit unfortunate in my opinion; the problem occurs for functions that contain a type in the name as well.
  Furthermore, primitive types do not use Pascal case ...)
? The `MonteCarloUnit` became a mod with free functions, because Rust has a task-local rng, which is nice.
  However, the syntax for generating numbers in a closed interval is a bit convoluted.
  Sure, the fancy type system allows you to write `rand::random::<Closed01<f32>>()`,
  but is that really better than having multiple methods like `random::half_open::<f32>()` and `random::closed::<f32>()`?
x I just ran `cargo run` on a fresh clone on Arch. It was _fast_.
  Like, compiled and run _instantly_!
  Running it with `time` tells me it took only 0.42 seconds. I should test it on Windows, but it not nearly that fast, I think.
  Edit: on Windows, it now runs in 1.16 seconds (after a clean), though there is more code at this point (3b893bfdc728e9bc2b56c6e96345ea5aed75e4ea).
  Edit2: on Arch, the same commit runs in 0.46 seconds.
x The method call syntax on numbers still feels unnatural. I guess it takes time to get used to it. Maybe it will be even better than the regular way.
x The `Intersect` method on `Surface` previously returned a boolean, and it had an out parameter.
  Now it is `intersect(&self, ray: &Ray) -> Option<Intersection>`. Much better!
- I should compare the occurrences of `mut` in Rust and `const` in C++. I think it makes sense to have immutability by default.
- Automatic dereferencing is nice. I rarely have to dereference anything manually, even though references are all over the place.
- Tuples are nice as well. `GetIntersections` on `Sphere` would return a boolean, and it had two out parameters.
  Now it returns `Option<(f32, f32)>`. Much better!
x I have not been using `Option` to replace `null` so far, but to replace out parameters.
  That has more to do with my C++ coding style though, I think.
- The per-module visibility system makes sense. Better than private/protetcted/public. It is like `internal` in C#.
  In C#, internal feels like a hack because there are objects everywhere,
  but with structs, traits and functions in Rust, it feels very natural.
  E.g. I can define helper functions that are not exposed outside of the module.
- I should compare compilation times for C++ and Rust when it is done.
x The `Object` type, which had three pointers, one of which would always be null, became an enum with two variants, for now.
  Edit: no, it actually the `MaterialBox` became an enum variant.
x Using `x as T` for casting is _much_ nicer than C++ `static_cast<T>(x)`.
x I wanted to use macros to avoid repetition in `PlotUnit` buffer setting.
  Unfortunately, a macro cannot expand to multiple statements, issue #10681.
  The workaround I used, is to make a the macro return a tuple.
  Also, macros do not capture their environment, and I need to enable the feature from `main.rs`, it does not work on a per-file basis. (I think.)
  Edit: chainging from `[f32]` to `[Vector3]` made this problem obsolete.
- Dead code warnings are nice, but they seem to be transitive, so now I get three pages of dead code warnings,
  while really only a few top-level functions are not used (yet).
  The warnings can be disabled with `#![allow(dead_code)]` in `main.rs`.
- The cryptic error messages are back:
      error: type `core::iter::Map<'_,&[f32],f32,core::slice::Chunks<'_,f32>>` does not implement any method in scope named `sum`
      error: binary operation `/` cannot be applied to type `core::iter::Map<'_,f32,f32,core::iter::Map<'_,&[f32],f32,core::slice::Chunks<'_,f32>>>`
? Iterators have no extra layer of indirection like C# has (`IEnumerable.GetEnumerator`).
  This means that it is harder to re-use a `map`.
  It is possible to do an intermediate `collect`, but in this case, I think the computation is cheaper than the memory acces (thought I did not measure).
  When using `collect().iter()`, the iterator now returns references instead of values.
x Iterators _do_ make it a lot more convenient to implement `TonemapUnit::FindExposure`, even with the flaw mentioned above.
  More functional, less loops. Like LINQ in C#.
  On the other hand, C++ has std::accumulate.
- The operator `+=` cannot be overloaded. Apparently the compiler does not replace it with a load, +, store. That is a shame.
x All these lifetimes and borrowing really make you consider ownership. I think this is a good thing.
  All other languages have it as well, but it is mostly implicit, and you can violate the contract. Rust enforces it statically.
  I like that.
- Emphasize that for conclusion!
x However, I am having trouble with coupled ownership and mutability now.
  I want to share an immutable object between tasks ... how do I do it?
  Edit: after a journey through several designs, I now have a better solution.
  This kind of thing really forces you to think about what should go where,
  and in the end, I think it leads to a better design.
- I just pattern matched a `Box<X>` as `ref mut`. The function that I want to pass it to expects a `&mut X`.
  So I did `&mut **x`, which feels extremely dirty. Is there a better way?
- The `Mutex` approach is really nice. Lock it and you get back the object in a wrapper,
  and when it goes out of scope, it unlocks.
  I think a very powerful feature is the `Deref` trait, which allows for something similar to extension methods in C#.
  (Now I think of it, you should be able to do this in C++ as well!)
- Having `new` as a function in the `impl` is great, because it makes `new` not special in any way,
  and you can have multiple “constructors” where none is the preferred one.
  This removes the need for things like static factory methods in C#.
x I do like the method syntax for mathematical functions. I really do.
x Not being able to use `min` and `max` on `f32` because `f32` is only partially ordered is correct, but inconvenient.
  Edit: I was just looking in the wrong places.
  Still, it can be a bit confusing.

Benchmarks
----------
Luculentus, msvc110, win64, regular:          0.20    +- 0.01    batches/sec.
Luculentus, msvc110, win64, PGO:              0.21    +- 0.01    batches/sec.
Luculentus, clang 3.4.2, Arch64, O3:          0.278   +- 0.01    batches/sec.
Luculentus, gcc 4.9, Arch64, O4:              0.310   +- 0.001   batches/sec.
Luculentus, gcc 4.9, Arch64, O4+march=native: 0.318   +- 0.01    batches/sec.
Robigo,     rustc 08-22, win32, O3:           0.189   +- 0.001   batches/sec.
Robigo,     rustc 08-21, Arch64, O3:          0.3203  +- 0.0003  batches/sec.
