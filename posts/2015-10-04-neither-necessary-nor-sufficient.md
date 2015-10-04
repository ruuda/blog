---
title: Neither necessary nor sufficient
date: 2015-10-04 15:20
---

A few days ago I stumbled upon a [post][when-rust-makes-sense]
that raised the following question:

> A language without garbage collection, in 2015?

I wrote a reply on Reddit,
but I thought I’d take the time to elaborate a bit more
in the form of a blog post.
The point can be summarised succinctly
by quoting [Bjarne Stroustrup][bjarne-quote]:

> Garbage collection is neither necessary nor sufficient.

[when-rust-makes-sense]: https://m50d.github.io/2015/09/28/when-rust-makes-sense.html
[bjarne-quote]:          https://isocpp.org/blog/2015/09/bjarne-stroustrup-announces-cpp-core-guidelines

<!--more-->

So a language without garbage collection, in 2015.
Why?
Because garbage collection does not solve the deeper underlying problem:
_resource management_.
Performance differences aside
(performance can mean different things in different scenarios
and there are a lot of myths surrounding GC performance
— I don’t want to go down that rabbit hole here),
a garbage collector only manages memory.
This works well for memory,
because there generally is more memory available than what is actually needed,
and (except for really low level stuff) applications do not care
about the actual address space they use.
An array does not care if it is stored at `0x3a28213a` or `0x6339392c`.
If something that is no longer alive was stored at `0x3a28213a`,
the array is happy with being stored at `0x6339392c`
if the garbage collector has not yet discovered that `0x3a28213a` is free.

The story is different when there is contention for a resource.
From the lock protecting a critical section to the socket serving a website
— you cannot afford to leave such resources lingering around
until a GC comes along to decide what is still being used.
In most of the “modern” languages sporting a GC,
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
There are constructs that can help in many cases.
Python has `with`, C# has `using`, D has `scope`, and Haskell has `bracket`.
These constructs bind resource lifetime to scope,
so consequently they cannot be used
when the resource has to outlive the current scope.

[real-world-haskell]: http://book.realworldhaskell.org/read/io.html#io.files

I buy the point that locks should not always be considered resources.
In specialised cases manually acquiring and releasing resources
might be the best option.
There neither a garbage collector
nor some form of automatic resource mangement can help.

So yes, I want a language without garbage collection.
Because I want a language that can do _resource management_.

