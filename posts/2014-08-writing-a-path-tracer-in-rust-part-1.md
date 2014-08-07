---
title: Writing a path tracer in Rust, part 1
date: 2014-08-01 21:00
---

- Papercuts and major design flaws in languages like C++ and C#.
- Rust appears to solve the flaws that I find most important.
- Wanted to try Rust for a very long time.
- Porting Luculentus to Rust.
- Also updating Luculentus codebase from 2012 (when C++11 was not widely available) to use idiomatic C++11.
- Destructors in C++ code should be gone.
- Conclusion? Performance comparison?

Porting specifics:
- Cargo is really, really nice. Easy to set up, and then just `cargo run` after writing some code.
- The compiler has mostly helpful error messages. Lots of warnings, which is good as well.
- I still find the Egyptian brackets a bit awkward, but it is mostly a matter of preference, and I think I will get used to it.
- It seems to be impossible to have multiple implementations of `Mul<T, U>`. I encountered this when trying to allow scalar multiplication for vectors on both sides.
  With some help from IRC (they were really quick), I found a workaround, but it does not solve the problem in all cases.
  Note that this essentially requires overloading based on traits, if it would be solved in a generic manner.
- The module structure and file layout can be a bit confusing. I had `vector3.rs` and `quaternion.rs`.
  Then `quaternion.rs` requires `vector3.rs`. To resolve, I had to add use statements to `main.rs`.
  Being used to C# this does not really make sense, but if you treat `mod` declarations as C++ `#includes`,
  and if you are aware that `main.rs` is the only file that matters to the compiler,
  then it does make sense.
- Having to use method call syntax for mathematical functions feels weird. Is there an other way?
- I _really_ dislike the Egyptian brackets! They make everything look unbalanced. :(
- If structs use Pascal casing, but files use lower case, how should I name the file and mod for `MonteCarloUnit`?
  The standard library itself is inconsistent. Libcollections has `TreeMap` in `treemap.rs`, but `PriorityQueue` in `priority_queue.rs`.
  I will use snake case for filenames and module names then, it seems nicer.
  (The casing conventions in Rust are a bit unfortunate in my opinion; the problem occurs for functions that contain a type in the name as well.
  Furthermore, primitive types do not use Pascal case ...)
- The `MonteCarloUnit` became a mod with free functions, because Rust has a task-local rng, which is nice.
  However, the syntax for generating numbers in a closed interval is a bit convoluted.
  Sure, the fancy type system allows you to write `rand::random::<Closed01<f32>>()`,
  but is that really better than having multiple methods like `random::half_open::<f32>()` and `random::closed::<f32>()`?
