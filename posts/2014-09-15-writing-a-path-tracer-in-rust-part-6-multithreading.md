---
title: Writing a path tracer in Rust, part 6: multithreading
date: 2014-09-15 19:04
---

As a learning exercise, I am porting the [Luculentus][luculentus] spectral path tracer to [Rust][rust].
You can follow the port on [GitHub][robigo-luculenta].
This post will outline how work is distributed among cores,
how synchronisation is handled,
and I will highlight some of the differences between C++ and Rust.

[rust]:             http://rust-lang.org
[luculentus]:       https://github.com/ruud-v-a/luculentus
[robigo-luculenta]: https://github.com/ruud-v-a/robigo-luculenta

Parallelism
-----------
Luculentus is a simple path tracer.
It does not use advanced algorithms like [Metropolis light transport][mlt],
and there is no [_k_-d tree][kdtree] to speed up scene intersection.
The path tracer could be made significantly faster, at the cost of more complex algorithms.
Another way to get more performance with little extra complexity,
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
After performing the task, it will ask the task scheduler for a new one,
and this will continue until the program stops.

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
    private:
      std::queue<int> availableTraceUnits;
      std::queue<int> doneTraceUnits;
      std::queue<int> availablePlotUnits;
      std::queue<int> donePlotUnits;
      bool gatherUnitAvailable;
      bool tonemapUnitAvailable;

    public:
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
or there would have to be distinct task types.
That would be more like the Rust approach,
which is as follows:

```rust
pub enum Task {
    Sleep,
    Trace(Box<TraceUnit>),
    Plot(Box<PlotUnit>, Vec<Box<TraceUnit>>),
    Gather(Box<GatherUnit>, Vec<Box<PlotUnit>>),
    Tonemap(Box<TonemapUnit>, Box<GatherUnit>)
}
```

Enums in Rust can have members.
This way, the invariants of the type can be better expressed:
in C++, the `unit` and `otherUnits` fields can be accessed even if the type is `Sleep`.
In that case, the values are undefined.
In Rust, these fields simply do not exist for a `Sleep` task.
The enum members are unnamed.
The only way to access them is via pattern matching.
The task scheduler is implemented like this:

```rust
pub struct TaskScheduler {
    available_trace_units: RingBuf<Box<TraceUnit>>,
    done_trace_units: RingBuf<Box<TraceUnit>>,
    available_plot_units: RingBuf<Box<PlotUnit>>,
    done_plot_units: RingBuf<Box<PlotUnit>>,
    gather_unit: Option<Box<GatherUnit>>,
    tonemap_unit: Option<Box<TonemapUnit>>,
    ...
}

impl TaskScheduler {
    pub fn get_new_task(&mut self, completed_task: Task) -> Task;
    ...
}
```

When writing the C++ version, I never really thought about ownership.
The task scheduler owns the units, and the queues and tasks refer to the units by index.
In Rust, the actual units are passed around.
When a trace task is created, a trace unit is taken from `available_trace_units` queue, and it is moved into the task.
The task now owns the unit, and and it is _gone_ from the task scheduler.
This way, it is impossible for the task scheduler to hand out the same unit to different worker threads.
When the worker thread requests a new task, it returns the completed task back to the task scheduler,
which will place its units in the done queue.
The gather unit and tonemap unit are handled in a similar way:
when they are handed out in a task,
the variables will be `None` until the units are returned.
The compiler can enforce this: `Tonemap` needs two boxes, and boxes can only be moved, not cloned.
Note that passing the units around is not inefficient, because only the boxes are moved, and a box is just a pointer.
The units themselves are constructed on the heap, and they are never moved.

These benefits of ownership are not unique to Rust:
the C++ version could have used `unique_ptr` (the equivalent of `Box`).
Instead of having one task type, it could have been a base class with a virtual `Execute` method.
This would allow for a version that is just as type-safe as the Rust version,
but it would be more cumbersome to write.

Executing Tasks
---------------
The C++ version dispatches on the task type.
The methods that actually perform the work are expected not to access units that are not associated with the task,
but the compiler does not prevent us from writing code that does that.

```cpp
void Raytracer::ExecuteTask(const Task task)
{
    switch (task.type)
    {
        case Task::Sleep:   ExecuteSleepTask(task);   break;
        case Task::Trace:   ExecuteTraceTask(task);   break;
        case Task::Plot:    ExecutePlotTask(task);    break;
        case Task::Gather:  ExecuteGatherTask(task);  break;
        case Task::Tonemap: ExecuteTonemapTask(task); break;
    }
}
```

If we forget to handle a case, the code would still compile fine, though most compilers would issue a warning.
In Rust, the dispatch is more involved, because it simultaneously extracts the units from the task.
This way, the functions that perform the work can only access the intended units.

```rust
fn execute_task(task: &mut Task,
                scene: &Scene,
                img_tx: &mut Sender<Image>) {
    match *task {
        Sleep =>
            App::execute_sleep_task(),

        Trace(ref mut trace_unit) =>
            App::execute_trace_task(scene, &mut **trace_unit),

        Plot(ref mut plot_unit, ref mut units) =>
            App::execute_plot_task(&mut **plot_unit, units[mut]),

        Gather(ref mut gather_unit, ref mut units) =>
            App::execute_gather_task(&mut **gather_unit, units[mut]),

        Tonemap(ref mut tonemap_unit, ref mut gather_unit) =>
           App::execute_tonemap_task(img_tx, &mut **tonemap_unit,
                                     &mut **gather_unit)
    }
}
```

In addition to the task, `execute_task` takes a `Scene` and `Sender<Image>`.
I talked about scene ownership in the [previous post][prev], and this is where it shows up.
The sender is not important for now.
Rust will force the match to be exhaustive, so there is no way to forget a case.
Managing ownership does get messy here.
Matching the box in the the task would move ownership, but we only want to borrow it.
That is what `ref` does.
We also want to mutate the contents of the box, because e.g. tracing modifies the unit, so we match with `ref mut`.
But then we have a `&mut Box<TraceUnit>`, and the function takes a `&mut TraceUnit`.
Dereferencing once yields a `Box<TraceUnit>`, and dereferencing a second time yields a `TraceUnit`.
The we must borrow it again, mutably, because the function expects a reference.
That is what `&mut **` is for.
It makes sense, but it feels awkward.

Synchronisation
---------------
There are only two points in the program that need synchronisation:
displaying the image in the user interface,
and retrieving a new task from the task scheduler.
Thread-safety of the task scheduler ensures that access to the units is thread-safe.
The only public method of the task scheduler is `GetNewTask`,
so that is the only place where synchronisation is required.
In C++, this is done by locking a mutex inside the method:

```cpp
Task TaskScheduler::GetNewTask(const Task completedTask)
{
    std::unique_lock<std::mutex> lock(mutex);

    ...
}
```

```cpp
task = taskScheduler.GetNewTask(task);
```

The lock is held while `lock` is in scope,
which is already a big advantage over manally acquiring and releasing the lock.
In Rust, the entire task scheduler is protected by a mutex.
The unusual thing about Rust’s mutex, is that it _owns_ the object it protects,
and the only way to get access to the object, is by acquiring the lock:

```rust
let ts = TaskScheduler::new(concurrency, image_width, image_height);
let task_scheduler = Arc::new(Mutex::new(ts));
```

```rust
task = task_scheduler.lock().get_new_task(task);
```

The mutex is put inside an arc, because it must be shared among threads.
Calling `lock()` on the mutex creates a mutex guard object,
similar to the C++ `unique_lock`.
It releases the lock when it goes out of scope.
However, now the only way to access the protected object is via the mutex guard.
This can be done by simply treating the mutex guard as the actual object, because it implements `Deref`.
This is similar to overriding `operator->` in C++.
(Remember that there is no distinction between `.` and `->` in Rust.)
The Rust mutex enforces that all access to the protected object is synchronised,
whereas in C++ you can forget to acquire the lock.
I think that a similar mutex could be built in C++,
but the one in the standard library just does not work that way.

The other part to synchronisation is displaying an image in the user interface.
Luculentus uses gtkmm for its interface, which only allows the UI to be updated from a designated UI thread.
The program uses a gtk-specific dispatcher,
to which callbacks are registered when the program starts.
When an image is ready, the worker thread puts it in a shared memory location,
and informs the dispatcher of the new image.
The UI event loop will execute the callback on the UI thread,
and display the image.

Rogigo Luculenta does not have a graphical user interface.
It saves the rendered image to a file.
The worker thread could do that,
but I wanted to do it form a designated thread, analogous to the UI thread in Luculentus.
Rust uses channels for communication between threads.
The worker thread puts the rendered image on the channel,
and an other thread waits for images, and writes them out.
This is what `img_tx` in `execute_task` is for: it is the sending part of the channel.

```rust
fn execute_tonemap_task(img_tx: &mut Sender<Image>,
                        tonemap_unit: &mut TonemapUnit,
                        gather_unit: &mut GatherUnit) {
    tonemap_unit.tonemap(gather_unit.tristimulus_buffer[]);

    let img = tonemap_unit.rgb_buffer.clone();
    img_tx.send(img);
}
```

After the main thread has started the worker threads,
it will go in a receive loop,
and write out an image using [LodePNG][lodepng] when it receives one:

```rust
loop {
    let img = images.recv();

    match lodepng::encode24_file(&Path::new("output.png"), img[],
                                 width as u32, height as u32) {
        Ok(_) => println!("wrote image to output.png"),
        Err(reason) => println!("failed to write output png: {}", reason)
    }
}
```

The call to `recv` will block until a new image is available.
Rust is more [honest][honest] than C++.
The fact that writing to a file might fail,
is reflected in the return type of `encode24_file`.
This forces you to consider failure,
whereas in C++ you can happily pretend that exceptions do not exists,
and the code will compile just fine.

[lodepng]: https://github.com/pornel/lodepng-rust
[honest]:  https://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-Functional-Programming

---

Ownership in Rust is a powerful tool.
It enables synchronisation primitives that are impossible to misuse.
Multithreading in Rust differs from C++ much in the same way that memory management does:
it is impossible to write something unsafe,
but it does require some careful thought to get to a point where the code compiles.
C++ and Rust have different ways of sending data across threads.
I like the channel approach, because channels are [value-oriented][values].
There are no channels in the C++ standard library,
so there it must be done via shared memory, which is place-oriented.
The tools that enable safe concurrent programming are there in both languages.
In C++, you can choose to use them.
In Rust, safety is enforced by default.

[values]: http://www.infoq.com/presentations/Value-Values

This concludes my port of the Luculentus spectral path tracer.
Next time I will summarise the process,
and I will compare the performance of the two versions ---
which might surprise you.

---

Discuss this post on [Reddit][reddit].
Rust 0.12.0-pre-nightly was used in this post.

[reddit]: http://reddit.com/r/rust/ruudvanasseldonk.com/2014/09/15/writing-a-path-tracer-in-rust-part-6-multithreading
