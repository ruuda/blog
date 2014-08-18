---
title: Writing a path tracer in Rust, part 4: tracing rays
date: 2014-08-18 13:00
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline scene intersection,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Rays
----
Last time I promised there would be rays.
In C++, they look like this:

```cpp
struct Ray
{
    Vector3 origin;
    Vector3 direction;
    float wavelength;
    float probability;
};
```

Because Luculentus is a spectral path tracer, every ray has an associated wavelength.
Interaction with surfaces depends on this wavelength.
Every ray also has an associated probability, which acts similar to colour components in a regular path tracer.
In Rust, the ray looks like this:

```rust
pub struct Ray {
    pub origin: Vector3,
    pub direction: Vector3,
    pub wavelength: f32,
    pub probability: f32
}
```

<!--more-->

Now we need something to intersect a ray with: a surface.
In C++, a surface is defined as follows:

```cpp
class Surface {
    public:
    virtual bool Intersect(const Ray ray,
                           Intersection& intersection) const = 0;
};
```

A virtual base class with an `Intersect` method.
The method takes an intersection by reference.
If the surface was intersected, it returns `true` and the intersection is filled with the details.
If the ray did not intersect the surface, it returns `false`.
An other approach would be to return an `Intersection*`, and return `null` when there is no intersection.
This would involve a heap allocation, so I opted for the first approach.

Rust has a much cleaner way to handle optional values: the `Option` type.
A surface in Rust is defined like this:

```rust
pub trait Surface {
    fn intersect(&self, ray: &Ray) -> Option<Intersection>;
}
```

Whereas in C++, surface classes derive from `Surface`, in Rust they implement a trait.
The `intersect` method returns some intersection if there was one, or `None` if nothing was intersected,
a much more natural approach than an out argument.
