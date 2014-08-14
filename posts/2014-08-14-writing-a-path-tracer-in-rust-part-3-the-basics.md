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
Note that there is no explicit `return`: by omitting a semicolon,
the last expression in a function determines the return value.
Initialization in Rust can be done as follows:

```rust
let v = Vector3 { x: 1.0, y: 1.0, z: 1.0 };
```

Note that the numbers do not need an `f` suffix, even though they are single-precision floats.
The type of a literal can depend on the context.

<!--more-->

Operator overloading
--------------------
There are a few obvious operators to overload for `Vector3`: binary + and -, unary -, and binary * for scalar multiplication.
One way to overload + in C++ is this:

```cpp
inline Vector3 operator+(const Vector3 a, const Vector3 b)
{
    Vector3 sum = { a.x + b.x, a.y + b.y, a.z + b.z };
    return sum;
}
```

In Rust, overloading `+` involves implementing the `Add` trait.
Traits are like interfaces in C#.

```rust
impl Add<Vector3, Vector3> for Vector3 {
    fn add(&self, other: &Vector3) -> Vector3 {
        Vector3 {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z
        }
    }
}
```

Overloading * for scalar multiplication in C++ is straightforward: we just change the types.

```cpp
inline Vector3 operator*(const Vector3 a, const float f)
{
    Vector3 prod = { a.x * f, a.y * f, a.z * f };
    return prod;
}
```

In Rust, the `Mul` trait takes two type parameters just like `Add`: the type of the right-hand side, and the type of the result.

```rust
impl Mul<f32, Vector3> for Vector3 {
    fn mul(&self, f: &f32) -> Vector3 {
        Vector3 {
            x: self.x * *f,
            y: self.y * *f,
            z: self.z * *f
        }
    }
}
```

This looks a bit awkward, because `f` must be dereferenced:
the `Mul` trait dictates that that `mul` takes its arguments by reference.
Note `self` is automatically dereferenced in `self.x`, there is no `->` like in C++.

So now we can wite things like `v * f` where `v` is a vector and `f` a scalar.
Can we also implement `f * v`?
In C++ it is straightforward, just switch the arguments.
In Rust, I think it cannot be done at this point.
Multiplication is a `mul` call on the left hand side, so we would have to implement `Mul<Vector3, Vector3>` for `f32`.
Unfortuntely, the compiler allows only one implementation of `Mul` for a type, regardless of the type parameters for `Mul`.
Because regular multiplication of two `f32`s implements `Mul` already, we cannot implement it for `Vector3` any more.

---

Discuss this post on [reddit][reddit].

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/14/writing-a-path-tracer-in-rust-part-3-the-basics
