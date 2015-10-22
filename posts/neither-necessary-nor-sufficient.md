---
title: Neither necessary nor sufficient
date: 2015-10-06
minutes: 5
---

A few days ago I stumbled upon a [blog post][when-rust-makes-sense]
that raised the following question:

> A language without garbage collection, in 2015?

The language referred to is Rust,
but that is hardly relevant here.
I wrote a reply on Reddit,
but I thought I’d take the time to elaborate a bit more
in the form of a blog post.
Quoting [Bjarne Stroustrup][bjarne-quote]
the point can be summarised succinctly:

> Garbage collection is neither necessary nor sufficient.

[when-rust-makes-sense]: https://m50d.github.io/2015/09/28/when-rust-makes-sense.html
[bjarne-quote]:          https://isocpp.org/blog/2015/09/bjarne-stroustrup-announces-cpp-core-guidelines

<!--more-->

A language without garbage collection, in 2015.
Why?
Because garbage collection does not solve the deeper underlying problem:
_resource management_.
Performance differences aside
(there are many different metrics for performance
and even more myths surrounding those
— I don’t want to go down that rabbit hole here),
a garbage collector only manages memory.
This works well for memory,
because there generally is more memory available than what is needed,
and applications do not care about the actual addresses they use.
The address space is uniform.
An array does not care if it is stored at `0x3a28213a` or `0x6339392c`.
If something that is no longer alive was stored at `0x3a28213a`,
the array is happy with being stored at `0x6339392c`
if the garbage collector has not yet discovered that `0x3a28213a` is free.

The story is different when there is contention for a resource.
From the lock protecting a critical section to the socket serving a website
— you cannot afford to leave such resources lingering around
until a GC comes along to decide what is still being used.
In most languages sporting a GC,
resource management is still utterly manual.
You still need to `close()` your `OutputStream` in Java.
You still need to `Release()`, `Close()` or `Dispose()` of your `Semaphore` in C#.
Even this Haskell example straight from [Real World Haskell][real-world-haskell]
is nothing more than fancy syntax for C's file handles:

```haskell
main = do
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh
```

The problem with disposable objects or handles,
is that they decouple resource lifetime from object lifetime.
This allows for programming errors such as writing to a closed socket
or not releasing a lock.

There exist constructs that can help in many cases.
Python has `with`, C# has `using`, and Haskell has `bracket`.
These constructs bind resource lifetime to scope.
A scope-based solution is often sufficient,
but in some cases a resource has to outlive the current scope.
A `using` block for instance,
is of no use for disposable member variables.

[real-world-haskell]: http://book.realworldhaskell.org/read/io.html#io.files

Of course, sometimes manual control of resoures is required.
A device driver _does_ care about actual addresses,
and manual acquire and release calls might be clearer than scope-based locking
for a thread-safe cache protected by a read-write lock.
In the specialised cases where there is a need for manual resource management,
neither garbage collection nor automatic resource mangement can help.
But little code benefits from manual memory management,
and I would argue that this is the case for other resourses as well.

So yes, I want a language without garbage collection.
Because I want a language that can do _resource management_.

