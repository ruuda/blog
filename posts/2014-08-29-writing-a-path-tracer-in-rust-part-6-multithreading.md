---
title: Writing a path tracer in Rust, part 6: multithreading
date: 2014-08-28 21:43
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline how work is distributed among cores,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Parallelism
-----------
Luculentus is a simple path tracer.
It does not use advanced algorithms like [Metropolis light transport][mlt],
and tere is no [_k_-d tree][kdtree] to speed up scene intersection.
The path tracer could be made significantly faster, at the cost of more complex algorithms.
An other way to get more performance with little extra complexity,
is to just throw more computing power at the problem.

[mlt]:    https://en.wikipedia.org/wiki/Metropolis_light_transport
[kdtree]: https://en.wikipedia.org/wiki/K-d_tree

The path tracing process so far has been pretty straightforward:

1. Generate and trace some random rays.
2. Determine their contribution to the final image.
3. Mix the contribution with earlier contributions.
4. Convert that to an image that a monitor can display.

The first three steps are performed in a loop,
and once in a while the fourth step is performed to visualise the current render state.

This process can be parallelised with little synchronisation.
Threads can do the loops in parallel.
Only access to the buffer in which all contributions are accumulated, needs to be synchronised.
To track progress, something still needs to ensure that an image is generated periodically.

<!--more-->

The task scheduler
------------------
The task scheduler is responsible for determining what threads should do.
The program starts a number of worker threads,
and each thread asks the task scheduler for a task.
After performing the task, it will ask the task scheduler for a new one, etc.

The task scheduler owns a collection of units, which are described in the [previous post][prev].
Each task can have units associated with it.
The trace task for example, has one associated trace unit.
The plot task has one associated plot unit, and multiple trace units to plot.
The task scheduler maintains two queues for trace units:
a queue of units ready to trace,
and a queue of units that must be plotted before they can be re-used.
When a trace task is completed, the corresponding unit is put in the ‘done’ queue, waiting to be plotted.
When a plot task is completed, all corresponding trace units are put in the ‘available’ queue again.
There is also a queue of plot units,
one gather unit, and one tonemap unit.
Depending the on the number of units in every queue,
and the time since the image was last tonemapped,
the task scheduler will either hand out trace tasks, plot tasks, gather tasks, or tonemap tasks.

[prev]: /2014/08/24/writing-a-path-tracer-in-rust-part-5-tonemapping

In C++, the task scheduler has a vector of units.
The queues are then queues of integers, which contain indices into the vector.
This enables a generic `Task` type, but it is not very type safe.
Whether the gather unit and tonemap unit are available is indicated by a simple boolean.

```cpp
class TaskScheduler
{
    ...

    std::queue<int> availableTraceUnits;
    std::queue<int> doneTraceUnits;
    std::queue<int> availablePlotUnits;
    std::queue<int> donePlotUnits;
    bool gatherUnitAvailable;
    bool tonemapUnitAvailable;

    std::vector<TraceUnit> traceUnits;
    std::vector<PlotUnit> plotUnits;
    std::unique_ptr<GatherUnit> gatherUnit;
    std::unique_ptr<TonemapUnit> tonemapUnit;

    Task GetNewTask(const Task completedTask);

    ...
};
```

The task has an enum that indicates its type,
an associated unit index,
and a collection of indices of other associated units.
These fields are only used for trace tasks, plot tasks, and gather tasks.
The task itself is defined as follows:

```cpp
struct Task
{
    enum TaskType
    {
        Sleep,
        Trace,
        Plot,
        Gather,
        Tonemap
    }
    type;

    int unit;
    std::vector<int> otherUnits;
};
```

If the implementation of the task scheduler is correct,
it will never hand out the same index to more than one worker thread,
and if the implementation of the worker threads is correct,
they will not access units at indices other than the indices in the current task.
However, the compiler does not prevent us from writing incorrect code, which might lead to races.
There is an alternative approach,
where `Task` would store pointers to the units.
Because the units have distinct types, there would have to be several fields on a task,
and most of them would be unused.
(They could be combined in a union.)
That would be more like the Rust approach,
which is as follows:

```rust
TODO
```

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/08/29/writing-a-path-tracer-in-rust-part-6-multithreading
