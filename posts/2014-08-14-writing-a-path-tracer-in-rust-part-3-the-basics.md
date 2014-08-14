---
title: Writing a path tracer in Rust, part 3: the basics
date: 2014-08-14 21:08
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline the vector and quaternion implementations,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Vector3
-------
The vector shows up in virtually every program that manipulates geometry.
Luculentus has a fairly straightforward `Vector3`:

```cpp
struct Vector3
{
    float x, y, z;

    inline float MagnitudeSquared() const
    {
      return Dot(*this, *this);
    }
};
```

There are also methods `Magnitude` and `Normalise` that I omitted here.
C++ allows initialization like this:

```cpp
Vector3 v = { 1.0f, 0.0f, 0.0f };
```

The vector in Rust is very similar:

```rust
pub struct Vector3 {
    pub x: f32,
    pub y: f32,
    pub z: f32
}
```

Struct members are not public by default, and visibility can be specified for the struct as well.
In Rust, there is a clear separation between code and data.
Methods are defined in a separate `impl` block, outside of the struct:

```rust
impl Vector3 {
    pub fn magnitude_squared(self) -> f32 {
        dot(self, self)
    }
}
```

Again, there are more methods here that I omitted.
Initialization in Rust can be done as follows:

```rust
let v = Vector3 { x: 1.0, y: 1.0, z: 1.0 };
```

Note that the numbers do not need an `f` suffix, even though they are single-precision floats.
The type of a literal can depend on the context.

---

Discuss this post on [reddit][reddit].

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/14/writing-a-path-tracer-in-rust-part-3-the-basics
