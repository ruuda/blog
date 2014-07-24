---
title: Continuations revisited
date: 2013-08-12 21:00
---

A while ago, I wrote about how `Task<T>` [is a monad](/2013/05/01/the-task-monad-in-csharp),
and about how that simplifies composition. Today, I want to look at observables,
and how they can be used as a replacement for tasks.

<!--more-->

Observables, which are part of the Reactive Framework (Rx), are not yet
included in the .NET base class library. The easiest way to get them is
through [NuGet](https://www.nuget.org/packages/Rx-Main). A lot has been
written about Rx already, and there are some great videos on Channel 9
as well. `IObservable<T>` [is the dual](http://channel9.msdn.com/shows/Going+Deep/E2E-Erik-Meijer-and-Wes-Dyer-Reactive-Framework-Rx-Under-the-Hood-1-of-2/)
of `IEnumerable<T>`. If `IEnumerable<T>` is a ‘pull’-based model, then
`IObservable<T>` is ‘push’. It can push values to an observer, as well as
an ‘end-of-sequence’ signal or an error.

Tasks vs. observables
---------------------
Let’s comare this to `Task<T>`. A `Task<T>` represents a value of `T` that is
either available, or will be available in the future. Additionally, the task
catches exceptions, so instead of resulting in a value in the future, it
might result in an exception instead. This maps directly to observables:
an observable represents values that will be pushed in the future, but
it might push an exception instead. The difference is that an `IObservable<T>`
might push multiple values of `T`, whereas a `Task<T>` will result in at
most one value of `T`.

A task can be continued with a delegate: the delegate will be called when
the task is completed. This is exactly a push-based model! You register
a continuation in advance, and the task will call it when the value is
available. With observables, you subscribe an observer, on which the
`OnNext` method will be called when the value is ready. There is a
striking similarity between tasks and observables here, and it seems
that ‘one-shot observables’ (which push only one value) can replace
tasks. This is indeed the case, and Rx even provides extension methods
to convert between the two.

Let’s consider the scenario of the previous post again, but now using observables.
We have classes `A`, `B`, `C`, and `D`, and the following methods:

```cs
IObservable<B> Method1(A a) { ... }
IObservable<C> Method2(B b) { ... }
IObservable<D> Method3(C c) { ... }
```

I am looking for an elegant composition operation that allows me to chain
these methods together. `Task<T>` turned out to be a monad, and the
monadic composition operation (bind), was exactly the operation that I
was looking for. `IObservable<T>` is a monad as well, so maybe its
implementation of bind is what I am looking for here? The method with
the right signature is called `SelectMany`. Given an `IObservable<T>`
and a `Func<T, IObservable<U>>`, it creates an `IObservable<U>`. It
turns out, that this is indeed the correct composition operation, and
that it represents continuations! Composition is as easy as:

```cs
IObservable<D> Composed(A a)
{
  return Method1(a).SelectMany(Method2).SelectMany(Method3);
}
```

The method name is not very descriptive here, and using `SelectMany` to
do `Then` seems rather odd. `SelectMany` is a LINQ operator inherited
from enumerables, so we are stuck with it. Note that because Rx is so
widely applicable it _cannot_ have one name that describes every
behaviour of bind. (Therefore it is usually called ‘bind’, which has
little intrinsic meaning outside of monads. Haskell even avoids all
intrinsic meaning by using a symbol.) In practice, adding aliases for
existing methods ultimately leads to more confusion, so I would **not**
recommend to make an extension method `Then` which is a `SelectMany` in
disguise. (For a real life example of methods that make things “easier”:
when should you use `Stream.Close`, and when `Stream.Dispose`? Hint: the
documentation for framework [4.0](http://msdn.microsoft.com/en-us/library/system.io.stream.close%28v=vs.100%29.aspx)
differs from [4.5](http://msdn.microsoft.com/en-us/library/system.io.stream.close%28v=vs.110%29.aspx).)

On a side note: as opposed to `Task<T>`, `IObservable<T>` _does_
implement the full set of monadic operations by default. The
implementation of ‘return’ — which takes a `T` and returns an
`IObservable<T>` — is simply called `Return`.

More composition
----------------
Let’s consider a different scenario, and solve it with tasks as well as observables.
Given a list of objects (say, of type `A`), first appy method `M1` to them in
parallel. When all of the calls are done, apply method `M2` in parallel,
and then block until all those calls are done.

Even though the Task Parallel Library has `ContinueWhenAll`, I still find this
clumsy without C# 5 `await` support:

```cs
IEnumerable<A> objects = ...

var tasks1 = objects.Select(a => Task.Factory.StartNew(() => a.M1()));
var task = Task.Factory.ContinueWhenAll(tasks1.ToArray(), _ =>
{
  var tasks2 = objects.Select(a => Task.Factory.StartNew(() => a.M2()));
  Task.WaitAll(tasks2.ToArray());
});

// Do something while the tasks are running, then wait.
task.Wait();
```

Again, tasks lack a composition operation, the current solution does not scale
well. What if I wanted to call `M1` … `M7` sequentially (but the methods applied
to the instances in parallel)? Furthermore, the continuation blocks, which wastes
a thread. Let’s try that again, using the `Then` method from the [previous post](/2013/05/01/the-task-monad-in-csharp):

```cs
Func<IEnumerable<A>, Action<A>, Task> doParallel = (xs, action) =>
{
  var tcs = new TaskCompletionSource<object>();
  var tasks = xs.Select(a => Task.Factory.StartNew(() => action(a)));
  Task.Factory.ContinueWhenAll(tasks.ToArray(), _ => tcs.SetResult(null));
  return tcs.Task;
};

IEnumerable<A> objects = ...

var task =  doParallel(objects, a => a.M1())
.Then(() => doParallel(objects, a => a.M2()));

// Do something while the tasks are running, then wait.
task.Wait();
```
    
This does scale to more continuations, and it does not waste a thread. It still
has a problem though: exceptions are not handled properly. It would require a lot
of boilerplate code to solve this quite trivial problem. .NET 4.5 improves
the situation a bit, there you can use:

```cs
Func<IEnumerable<A>, Action<A>, Task> doParallel = (xs, action) =>
{
  var tasks = xs.Select(a => Task.Factory.StartNew(() => action(a)));
  return Task.WhenAll(tasks.ToArray());
};
```

This will handle exceptions correctly as well. There might be a better
solution that I am unaware of though, please let me know if you have one!

How about observables? I find this version more attractive than its TPL
equivalent:

```cs
IEnumerable<A> objects = ...

var observables = Observable.Concat
(
  objects.ToObservable().SelectMany(a => Observable.Start(() => a.M1())),
  objects.ToObservable().SelectMany(a => Observable.Start(() => a.M2()))
)
.Publish(); // Do not wait until subscription.

// Do something while the observables are running, then wait.
observables.Wait();
```
    
It scales well to many continuations, it is simple, and it handles exceptions
properly. One thing I find remarkable, is that Rx was not specifically designed
for this scenario: it provides elegant solutions to many problem domains!

Note that in this case, PLINQ is even easier, if you do not need the 
calling thread to do anything in parallel with the calls:

```cs
IEnumerable<A> objects = ...

objects.AsParallel().ForAll(a => a.M1());
objects.AsParallel().ForAll(a => a.M2());
```

However, PLINQ only provides a solution to very basic problems. What if there
was an `M1Async`, that returns either a `Task` or `IObservable<Unit>`? Converting
it to a synchronous method by appending `.Wait()` works, but in the case of
PLINQ this wastes a thread per concurrent call. The TPL and Rx ways above
actually become simpler for `M1Async`, because asynchronicity need not be
introduced explicitly any more.

TL;DR: although the Task Parallel Library greatly simplified asynchronous
programming in C#, observables can do anything tasks can do, with the additional
benefit of better composability.
