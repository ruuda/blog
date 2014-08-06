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
