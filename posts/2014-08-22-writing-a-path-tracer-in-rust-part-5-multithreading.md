---
title: Writing a path tracer in Rust, part 5: multithreading
date: 2014-08-22 11:40
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline how work is distributed among cores,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Trace units
-----------
There are multiple things that a spectral path tracer must do,
and tracing rays is only one of them.
In Luculentus, these tasks have _units_ associated with them.
The first unit is the _trace unit_.
It chooses a wavelength in the visible spectrum and a point on the screen at random.
The camera converts these into a ray.
In the previous post I outlined how materials and surfaces are then used to determine the intensity of the given ray.
The trace units does this a fixed number of times, and stores the screen position,
wavelength, and intensity in a buffer.

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

TODO: discuss at random afterwards?

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

In the C++ version, every trace unit has a pointer to the scene.
The scene is constructed once and it is immutable afterwards,
so it can be safely read from multiple threads.
It is only deleted after all trace units have stopped rendering.
I failed to write that in Rust (if you know how, please let me know),
so in Rust, every worker thread has its own copy of the scene,
and it is passed explicitly to the trace unit.

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

[ciexyz]:  https://en.wikipedia.org/wiki/CIE_XYZ
