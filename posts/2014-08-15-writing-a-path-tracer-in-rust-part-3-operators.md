---
title: Writing a path tracer in Rust, part 3: operators
date: 2014-08-15 12:00
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

There are also `Magnitude` and `Normalise` methods that I omitted here.
C++ allows initialization like this:

```cpp
Vector3 v = { 1.0f, 0.0f, 0.0f };
```

The vector in Rust is similar:

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

In Rust, overloading + involves implementing the `Add` trait.
Traits are like interfaces in C#.
`Add` takes two generic parameters: the type of the right-hand side, and the type of the result.
Both are `Vector3` in this case.

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

Overloading * for scalar multiplication in C++ is similar to addition:

```cpp
inline Vector3 operator*(const Vector3 a, const float f)
{
    Vector3 prod = { a.x * f, a.y * f, a.z * f };
    return prod;
}
```

In Rust, the `Mul` trait takes two type parameters as well: the type of the right-hand side, and the type of the result.

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
Note that `self` is automatically dereferenced in `self.x`, there is no `->` like in C++.

Now we can wite things like `v * f` where `v` is a vector and `f` a scalar.
Can we also implement `f * v`?
In C++ it is straightforward, just switch the arguments.
In Rust, I think it cannot be done at this point.
Multiplication is a `mul` call on the left hand side, so we would have to implement `Mul<Vector3, Vector3>` for `f32`.
Unfortuntely, the compiler allows only one implementation of `Mul` for a type, regardless of the type parameters for `Mul`.
Because regular multiplication of two `f32`s implements `Mul` already, we cannot implement it for `Vector3` any more.

Quaternions
-----------
Luculentus uses quaternions to represent rotations.
Most of the `Quaternion` implementation is similar to that of `Vector3`, but with four components instead of three.
(In fact, quaternions form a vector space, so they _are_ vectors.)
The interesting thing is that quaternions support quaternion multiplication in addition to scalar multiplication.

In C++, we can implement them both:

```cpp
inline Quaternion operator*(const Quaternion q, const float f)
{
    Quaternion prod = { q.x * f, q.y * f, q.z * f, q.w * f };
    return prod;
}

inline Quaternion operator*(const Quaternion a, const Quaternion b)
{
    Quaternion prod =
    {
        a.w * b.x  +  a.x * b.w  +  a.y * b.z  -  a.z * b.y,
        a.w * b.y  -  a.x * b.z  +  a.y * b.w  +  a.z * b.x,
        a.w * b.z  +  a.x * b.y  -  a.y * b.x  +  a.z * b.w,
        a.w * b.w  -  a.x * b.x  -  a.y * b.y  -  a.z * b.z
    };
    return prod;
}
```

As we saw before, we cannot implement `Mul` twice in Rust.
Luckily, the IRC channel was very helpful, and there is a solution.
The trick is to define a trait for “things that can be the right-hand side of multiplication with a quaternion”:

```rust
trait MulQuaternion {
    fn mul(&self, lhs: &Quaternion) -> Quaternion;
}
```

Then we can implement `Mul` where the right-hand side must implement `MulQuaternion`:

```rust
impl<T: MulQuaternion> Mul<T, Quaternion> for Quaternion {
    fn mul(&self, other: &T) -> Quaternion {
        other.mul(self)
    }
}
```

Finally, we can implement `MulQuaternion` for `f32` as well as `Quaternion` itself:

```rust
impl MulQuaternion for f32 {
    fn mul(&self, lhs: &Quaternion) -> Quaternion {
        Quaternion {
            x: lhs.x * *self,
            y: lhs.y * *self,
            z: lhs.z * *self,
            w: lhs.w * *self
        }
    }
}

impl MulQuaternion for Quaternion {
    fn mul(&self, lhs: &Quaternion) -> Quaternion {
        Quaternion {
            x: lhs.w * self.x + lhs.x * self.w + lhs.y * self.z - lhs.z * self.y,
            y: lhs.w * self.y - lhs.x * self.z + lhs.y * self.w + lhs.z * self.x,
            z: lhs.w * self.z + lhs.x * self.y - lhs.y * self.x + lhs.z * self.w,
            w: lhs.w * self.w - lhs.x * self.x - lhs.y * self.y - lhs.z * self.z
        }
    }
}
```

It feels a bit weird, because the second argument is the left-hand side of the multiplication.
Like with `Vector3`, I think it is impossible to implement scalar multiplication with the scalar on the left.
Please let me know if I am wrong!
There is [an RFC][rfc] for multidispatch in traits.
If it gets accepted, it might allow multiple implementations of `Add` and `Mul` for the same type.
The correct one would be selected based on the types of the operands (or method arguments in general).
That would certainly simplify the quaternion code, and it would allow scalar multiplication with the scalar on the left.

[rfc]: https://github.com/rust-lang/rfcs/pull/195

**Edit 2014-11-05:** Rust 0.13 does have multidispatch now,
and the code has been simplified accordingly.

---

Next time I will discuss more of the type system,
and there will finally be rays!
I will also discuss more of the internals of the path tracer.

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/15/writing-a-path-tracer-in-rust-part-3-operators
