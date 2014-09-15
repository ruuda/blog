---
title: Writing a path tracer in Rust, part 4: tracing rays
date: 2014-08-19 13:00
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
Last time I promised there would be rays, so here they are:

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
In C++, the intersection and surface are defined as follows:

```cpp
struct Intersection
{
    Vector3 position;
    Vector3 normal;
    Vector3 tangent;
    float distance;
};

struct Surface
{
    virtual bool Intersect(const Ray ray,
                           Intersection& intersection) const = 0;
};
```
An `Intersection` contains details about the surface at the intersection,
and the distance along the ray.
The distance is later used to pick the closest intersection.
A surface is just an abstract base class with an `Intersect` method.
The method takes an intersection by reference.
If the surface was intersected, it returns `true` and the intersection is filled with the details.
If the ray did not intersect the surface, it returns `false`.
An other approach would be to return an `Intersection*`, and return null when there is no intersection.
This would involve a heap allocation, so I opted for the first approach.

Rust has a cleaner way to handle optional values: the `Option` type.
The intersection and surface in Rust are defined like this:

```rust
pub struct Intersection {
    pub position: Vector3,
    pub normal: Vector3,
    pub tangent: Vector3,
    pub distance: f32
}

pub trait Surface {
    fn intersect(&self, ray: &Ray) -> Option<Intersection>;
}
```

Whereas in C++, surface classes derive from `Surface`, in Rust they implement a trait.
The `intersect` method returns some intersection if there was one, or `None` if nothing was intersected.
I find this to be a more natural approach than an out argument.
Note that even though this is like returning a pointer that might be null in C++,
there is no heap allocation involved here.

Materials
---------
Now we can intersect surfaces, but there is an other part to path tracing.
When a surface is intersected, the material at that point determines how the light path continues.
In C++, all non-emissive materials derive from `Material`:

```cpp
struct Material
{
    virtual Ray GetNewRay(const Ray incomingRay,
                          const Intersection intersection,
                          MonteCarloUnit& monteCarloUnit) const = 0;
};
```

The material takes the incoming ray and intersection details,
and produces a new ray.
This method need not be deterministic, so a Monte Carlo unit is provided as well,
which is a wrapper around a random number generator.
Every thread has its own Monte Carlo unit, so there is no race for random numbers.

In Rust, `Material` is a trait:

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
    std::shared_ptr<Surface> surface;
    std::shared_ptr<Material> material;
    std::shared_ptr<EmissiveMaterial> emissiveMaterial;
};
```

The `surface` pointer must never be null,
and either the material or emissive material must be non-null.
It works, but the compiler does not prevent you from creating an invalid object
that contains no material, or both a reflective and emissive material.
It could be improved a bit by using a tagged union, but for this simple case, two pointers suffice.
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

Enums in Rust are more than glorified integers: they enable [sum types][sumtype].
There is no way to assign both a reflective and emissive material to an object now,
and because Rust does not have null pointers,
there is also no way to assign _no_ material to an object.
Much better than the C++ version!

The `Box` is like a `unique_ptr` in C++.
Note that the types inside `Box` are the traits we defined above, not structs.
This is the way to do runtime polymorphism in Rust.
The value in the `Box` could have any type, provided that it implements the right trait.

[sumtype]: https://en.wikipedia.org/wiki/Algebraic_data_type

We now have everything to build and intersect a scene.
Luculentus is a simple proof-of-concept path tracer,
so there is no data structure for fast scene intersection.
The scene is just a vector of objects, and to intersect it,
we intersect all objects, and return the closest intersection.

Putting it together
-------------------
Given a ray, we would like to simulate a light path (from the camera backwards),
until a light source is hit.
Then we can compute the intensity of light along this path.

```cpp
Ray ray = camera.GetRay(monteCarloUnit);
float intensity = 1.0f;
do
{
    Intersection intersection;
    const Object* object = scene->Intersect(ray, intersection);

    if (!object) return 0.0f;

    if (!object->material)
    {
        // If material is null, emissiveMaterial is not null.
        return intensity *
            object->emissiveMaterial->GetIntensity(ray.wavelength);
    }

    ray = object->material->GetNewRay(ray, intersection, monteCarloUnit);
    intensity *= ray.probability;
}
while (...)
```

We intersect a ray with the scene.
If nothing was hit, the light intensity is zero --- a black background.
If an object was intersected, and its `material` pointer is null,
its `emissiveMaterial` is not null by assumption, so the object is a light source.
The final intensity is the intensity of the light source reduced by the effects of previous bounces.
If the `material` pointer was not null,
we ask the material to generate a ray that continues the path.
The loop continues with a probability that decreases with every intersection.
For simplicity, I omitted the details in the code.
Paths with a low intensity also have a higher chance of being terminated.
If the loop terminates, the intensity is just zero.

The Rust version uses pattern matching instead of null pointers:

```rust
let mut ray = camera.get_ray();
let mut intensity = 1.0f32;
loop {
    match scene.intersect(&ray) {
        None => return 0.0,
        Some((intersection, object)) => {
            match object.material {
                Emissive(ref mat) => {
                    return intensity * mat.get_intensity(ray.wavelength);
                },
                Reflective(ref mat) => {
                    ray = mat.get_new_ray(&ray, &intersection);
                    intensity = intensity * ray.probability;
                }
            }
        }
    }

    if ... { break; }
}
```

I find the C++ version more aesthetically pleasing and readable.
The `Emissive` and `Reflective` enum variants contain a `Box` with the material.
If we were to match on that, it would move the box into the match variable.
Here we do not want to take ownership of the material,
so by matching with `ref`, the `mat` variables will borrow the material instead.

---

For the cases in this post, the types in Rust and C++ are very similar.
However, Rust has a more advanced type system, with several benefits:
it prevents you from constructing invalid objects,
and it forces you to consider every case.
Next time I will discuss how ray intensities are converted into an image.

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/19/writing-a-path-tracer-in-rust-part-4-tracing-rays
