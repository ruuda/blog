---
title: Writing a path tracer in Rust, part 4: tracing rays
date: 2014-08-18 13:00
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline scene intersection and materials,
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
class Surface
{
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

Materials
---------
Now we can intersect surfaces, but there is an other part to path tracing.
When a surface is intersected, the material at that point determines how the ray bounces.
In C++, all materials derive from this class:

```cpp
class Material
{
    public:
    virtual Ray GetNewRay(const Ray incomingRay,
                          const Intersection intersection,
                          MonteCarloUnit& monteCarloUnit) const = 0;
};
```

The material takes the incoming ray and intersection details (which include the surface normal),
and produces a new ray.
This function need not be deterministic, so a Monte Carlo unit is provided as well,
which contains a random number generator.
Every thread has its own monte carlo unit, so there is no race for random numbers.

In Rust, material is a trait:

```rust
pub trait Material {
  fn get_new_ray(&self,
                 incoming_ray: &Ray,
                 intersection: &Intersection)
                 -> Ray;
}
```

Rust has a task-local random number generator,
so there is no need to provide one explicitly: random number generation cannot race.

Besides a regular reflective material, Luculentus also has an `EmissiveMaterial` for light sources.
`EmissiveMaterial` has one method that returns the light intensity for a given wavelength.
The Rust trait is similar to the abstract class in C++.
This approach is great when the light source has a broad spectrum (like the sun or a light bulb),
but it does not work for spectra with only a few spectral lines (like a natrium lamp).
Because wavelengths are chosen at random, the probability of hitting a spectral line is too low.
This could be compensated for by not choosing wavelengths at random,
but Luculentus is not that advanced.

Objects
-------
The scene in Luculentus consists of _objects_.
Objects have some geometry described by a `Surface`,
and they can either be emissive (for light sources) or reflective.
In C++, this is done as follows:

```cpp
struct Object
{
    Surface* surface;
    Material* material;
    EmissiveMaterial* emissiveMaterial;
};
```

The `surface` pointer must never be null,
and either the material or emissive material must be non-null.
It works, but the compiler does not prevent you from creating an invalid object
that contains no material, or both a reflective and emissive material.
It could be improved a bit by using a tagged union, but for this simple case, two pointers suffice.
Also, it would be more idiomatic to use a unique_ptr or shared_ptr instead of the raw pointers nowadays.
In Rust, valid objects can be enforced statically:

```rust
pub enum MaterialBox {
  Reflective(Box<Material>),
  Emissive(Box<EmissiveMaterial>)
}

pub struct Object {
  pub surface: Box<Surface>,
  pub material: MaterialBox
}
```

Enums in Rust are much more than glorified integers: they enable [sum types][sumtype].
There is no way to assign both a reflective and emissive material to an Object now,
and because Rust does not have null pointers,
there is also no way to assign no material to an Object.
Much better than the C++ version!

The `Box` is like a `unique_ptr` in C++.
Note that the types inside `Box` are the traits we defined above, not structs.
This is the way to do runtime polymorphism in Rust.
The value in the `Box` could have any type, provided that it implements the right trait.

[sumtype]: https://en.wikipedia.org/wiki/Algebraic_data_type

For the cases in this post, the types in Rust and C++ are very similar.
However, Rust provides much more compile-time safety: it prevents you from constructing invalid objects.
It can do this because of the more advanced type system.
Next time I will discuss multithreading and the task system in Luculentus.

---

Discuss this post on [Reddit][reddit].

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/18/writing-a-path-tracer-in-rust-part-4-tracing-rays
