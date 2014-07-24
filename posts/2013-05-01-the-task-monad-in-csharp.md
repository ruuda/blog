---
title: The task monad in C#
date: 2013-05-01 20:00
---

Today I was trying to compose several asynchronous methods in C#.
These methods all had one argument, and returned a `Task<T>`.
The idea was very simple: the first method returns a `Task<A>`.
The second method takes an `A` and returns a `Task<B>`.
The third method takes a `B`, etc. As it turns out, there is no easy
way to compose these methods in C#, even though there should be.

<!--more-->

The `System.Threading.Tasks` namespace is very well designed, and overall
everything seems to be thought through very well. However, in this
particular case, the framework seems to lack an obvious composition operation.
Consider the following scenario:

```cs
class A { ... }
class B { ... }
class C { ... }
class D { ... }

Task<B> Method1(A a) { ... }
Task<C> Method2(B b) { ... }
Task<D> Method3(C c) { ... }
```

I would like to compose these methods, to create a method with the following
signature:

```cs
Task<D> Composed(A a) { ... }
```

Before tackling the general case of _n_ methods, consider composing
`Method1` and `Method2`, to end up with the method:

```cs
Task<C> Composed12(A a) { ... }
```

Using the `ContinueWith` method, a naive composition operation might be
as follows:

```cs
Task<...> Composed12(A a)
{
  return Method1(a).ContinueWith(tb => Method2(tb.Result));
}
```

This works, but the method has the wrong return type. The result is now
wrapped twice in a task. To solve this issue, one might write:

```cs
Task<C> Composed12(A a)
{
  return Method1(a).ContinueWith(tb => Method2(tb.Result).Result);
}
```

However, this is not really a continuation: it blocks. The overall result
is the same, but as soon as the task returned by `Method1` is completed,
the continuation task will block, waiting for the result of `Method2`.
This essentially wastes a thread. One could use `Unwrap`, which does solve the
problem in many cases, but not when a task fails or when it is cancelled.

Bind
----
If you have ever done some functional programming, you might recognise
something here. `Task<T>` is a **monad**, and the composition operation I am
looking for is the monadic composition operation (usually called _bind_).
Given two methods, one that returns a `Task<B>`, and one that takes a `B`
and returns a `Task<C>`, composition should give me a method that returns a `Task<C>`.
This is exactly what bind does. In the context of tasks,
the name 'bind' might seem a bit odd. A more natural name in this case,
would be 'then'. Peculiar enough, the designers of `Task` did not include such
a method. Fortunately, [implementations of this method](https://blogs.msdn.com/b/pfxteam/archive/2010/11/21/10094564.aspx)
have been written. Using this `Then` method, composition becomes easy and elegant:

```cs
Task<C> Composed12(A a)
{
  return Method1(a).Then(Method2);
}

Task<D> Composed(A a)
{
  return Method1(a).Then(Method2).Then(Method3);
}
```

The default choice of continuing the task synchronously might not always be
desired, but this can easily be changed.

Return
------
For `Task<T>` to be a monad, it not only requires a 'bind' method,
but a 'return' method as well. The return method is nothing like the `return`
statement in C#. What it does, is convert a non-monadic value, into a
monadic value. In other words, it converts `T` into a `Task<T>`. The .NET
framework 4.5 implements return: it is called `Task.FromResult`. For
earlier versions, a manual implementation is very simple:

```cs
Task<T> Return<T>(T t)
{
  var tcs = new TaskCompletionSource<T>();
  tcs.SetResult(t);
  return tcs.Task;
}
```

In the case of my composed method above, this would allow one to write:

```cs
Task<D> Composed(A a)
{
  return Return(a).Then(Method1).Then(Method2).Then(Method3);
}
```

As you can see, this has little advantage over the original form. Moreover,
when the synchronous continuation implementation of `Then` is used,
this will block! There might still be cases where return could be useful though.
