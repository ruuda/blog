---
title: Writing a path tracer in Rust, part 5: tonemapping
date: 2014-08-22 11:40
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline how work is distributed among cores,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Units
-----
There are several things that a spectral path tracer must do,
and tracing rays is only one of them.
Eventually, the intensities of all the light paths have to be converted into an image.
In Luculentus, this is a multi-stage process, and every stage has its own unit that performs the work.

- A **trace unit** generates random camera rays and computes their intensities.
  It stores the results (which are called mapped photons) in an internal buffer.
- A **plot unit** converts the buffer to an image in the [CIE XYZ][ciexyz] colour space.
  It stores this image in an internal buffer.
- The **gather unit** accumulates the buffers of plot units into a single image,
  still in the CIE XYZ colour space.
- The **tonemap unit** determines the correct exposure for the image,
  and converts the image from CIE XYZ to the sRGB colour space.
  This is the final image.

Path tracing is an incremental process.
Rays are generated at random, and the more rays are rendered, the better the image will be.
Therefore multiple trace units and plot units are recycled in a loop:
the trace unit traces some rays, the plot unit plots them,
and the process continues.
To watch the result evolve,
the tonemap unit periodically generates an image.

[ciexyz]:  https://en.wikipedia.org/wiki/CIE_XYZ

<!--more-->

Trace units
-----------
The trace unit chooses a wavelength in the visible spectrum and a point on the screen at random.
The camera converts these into a ray.
In the [previous post][prev] I outlined how materials and surfaces
are then used to determine the intensity of the given ray.
The trace units does this a fixed number of times, and stores the screen position,
wavelength, and intensity (together a _mapped photon_) in a buffer.

[prev]: /2014/08/19/writing-a-path-tracer-in-rust-part-4-tracing-rays

In C++, it is done as follows:

```cpp
void TraceUnit::Render()
{
    for (auto& mappedPhoton : mappedPhotons)
    {
        const float wavelength = monteCarloUnit.GetWavelength();
        const float x = monteCarloUnit.GetBiUnit();
        const float y = monteCarloUnit.GetBiUnit() / aspectRatio;
        mappedPhoton.wavelength = wavelength;
        mappedPhoton.x = x;
        mappedPhoton.y = y;
        mappedPhoton.probability = RenderCameraRay(x, y, wavelength);
    }
}
```

Hidden in this piece of code, is that `TraceUnit` has a pointer to the scene
that is used by `RenderCameraRay`.
The scene is initialised once when the program starts, and it is immutable afterwards.
Because only reads are required to intersect the scene,
multiple threads can render the same scene simultaneously.
The scene is only deleted after all rendering has stopped.

In Rust, the main trace loop looks like this:

```rust
pub fn render(&mut self, scene: &Scene) {
    for mapped_photon in self.mapped_photons.mut_iter() {
        let wavelength = ::monte_carlo::get_wavelength();
        let x = ::monte_carlo::get_bi_unit();
        let y = ::monte_carlo::get_bi_unit() / self.aspect_ratio;
        mapped_photon.wavelength = wavelength;
        mapped_photon.x = x;
        mapped_photon.y = y;
        mapped_photon.probability =
            TraceUnit::render_camera_ray(scene, x, y, wavelength);
    }
}
```

I struggled a lot with who should own the scene in Rust.
At first, the `TraceUnit` had a pointer to the scene just like the C++ version,
but this required lifetime annotations everywhere:

```rust
pub struct TraceUnit<'s> {
    scene: &'s Scene,
    ...
}
```

When you store a pointer in a struct in Rust,
an explicit lifetime must always be specified.
There are no dangling pointers in Rust.
The compiler can ensure that because it knows the lifetime of the pointee, which is part of the type.
The type `&'s Scene` is a pointer to a `Scene` that is valid for the lifetime `'s`.
As you can see, `TraceUnit` now requires a lifetime parameter.
This is infectious, in the way that `const` is in C++: now everything that owns a `TraceUnit`
also takes a lifetime parameter, and suddenly there are lifetimes everywhere.

I struggeld some more with this, stumbling from compiler error to compiler error.
Rusts forces you to get ownership right, and I think in the end it also led to a better design.
I finally settled for the `render` method taking a pointer to the scene.
This moves burden of ownership to the caller of `render`.
As there can be various threads rendering, every thread has its own `Arc<Scene>`.
`Arc` is an atomically reference-counted pointer.
This allows the scene to be shared among threads,
and it will be deleted when there are no more arcs pointing to it.

One caveat here is that to use `Scene` with `Arc`, it has to implement the traits `Send` and `Sync`.
These traits cannot be implemented manually, only by the compiler.
A struct is `Send` and `Sync` if all its members are.
This way, the compiler enforces thread-safety at compile time.
However, `Scene` initially was not `Send` and `Sync`,
because it contains objects that contain a `Box<Surface>`.
A type implementing `Surface` need not implement `Send` or `Sync`,
so the box could not be proven thread-safe,
and therefore the scene could not be proven thread safe.
The resulution (thanks to the IRC channel again!) was to explicitly require the contents of the box
to be `Send` and `Sync`:

```rust
pub struct Object {
    pub surface: Box<Surface + Sync + Send>,
    pub material: MaterialBox
}
```

`MaterialBox` was also updated accordingly, and then everything compiled just fine.

Plot units
----------
The next thing to do is convert the wavelength to a colour, and plot a pixel to the canvas.
This is done by a _plot unit_.
Luculentus uses the [CIE XYZ][ciexyz] colour space internally, because it is a linear colour space.
This means that it is safe to treat colours as vectors, and add them together.
(This is not valid in e.g. sRGB.)
The plot unit has an internal canvas that starts out black.
The unit then loops trough all the photons in the buffer of the trace unit.
A lookup table is used to convert a wavelength into a CIE XYZ tristimulus value.
Two such tables exist: CIE 1931 and CIE 1964.
Luculentus implements them both, but I only ported the 1931 one.

---

Altough the C++ and Rust snippets in this post are very much alike,
there is a big difference:
the Rust code is guaranteed to be memory safe and thread safe.
The C++ code _may_ be memory safe and thread safe, but it need not be.
If your design was safe in the first place, these guarantees come at little extra cost.
However, the compiler refuses to compile anything that might be unsafe.
This forces you to think your design through up front.
You cannot just write some code and go and fix the memory leaks later on.
The compiler errors do point out valid problems in your code, and I think this guides you to the correct solution.
With Rust, I spent more time fixing compiler errors than I spent debugging runtime errors.
This is something that does not show in the final code.

