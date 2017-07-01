---
title: On benchmarking
date: 2017-07-01
minutes: ??
synopsis: ??
run-in: Measuring performance is hard
math: true
---

Measuring performance is hard.
Interpreting measurements may be even harder.
I recently stumbled upon a Reddit thread
where people attempted to compare performance of a few programs.
The results went something like this:

Program  Runtime (ms)
-------  ------------
A        6360
B        5116

Quickly others joined posting the results for their systems.
At some point somebody questioned the measurement setup.
But at no point did anybody question the conclusion
that B is the faster program.
Yet, from this data it is impossible to tell.
**A single sample for every scenario provides insufficient information.**

Suppose we would run the benchmark again, with the following results:

Program  Runtime (ms)
-------  ------------
A        6358
B        5123

Such a result would increase our confidence in the conclusion.
On the other hand, a result like the following would lead us to question it:

Program  Runtime (ms)
-------  ------------
A        5383
B        5721

With only a single measurement,
it is impossible to tell in which situation we are.
In the second case,
the presentation gives a false sense of millisecond precision,
and it is not clear what the conclusion should be.
Fortunately statistics can provide an answer here.

Measurement complications
-------------------------

I started this post by claiming that measuring performance is hard.
There are two main reasons for this.
The various sources of noise that affect measurements are one reason.
Examples include CPU frequency scaling,
thermal throttling,
cache trashing by other processes,
quantisation noise,
and many, many more that deserve a post of their own.
Once you are aware of these
it might be posible to eliminate or mitigate some of them.
Failing to do so will make it harder to draw conclusions from your data,
but if you get the statistics right,
that at least will not result in *wrong* conclusions.

The second reason is much more dangerous:
it is often not obvious that you are measuring the thing you think you are measuring.
Optimising compilers that eagerly optimise away your computation;
measuring disk or RAM bandwidth rather than compute performance;
and again many more complications that warrant an entire post of their own.
Often the question of what the thing to measure should be is left implicit.
Do you want to measure the best-case performance of a component in isolation,
with warm caches, nothing else running on the system, etc.?
Or do you want to measure performance in the typical use case?

This post is not a measurement guide.
For the remainder of the post I will assume
that we know exactly what we want to measure,
and that we have decided on a setup to measure that value reliably.
But even with a good setup, noise and imprecision are going to be inevitable.
We have to quantify them and deal with it.

A statistical test
------------------

Let’s say we set out to answer the question “is program B faster than program A?”.
In this form the question is still too vague.
When the runtime is different for every run,
simply comparing times could lead to an inconclusive situation
were in some runs A is faster,
and sometimes B is faster.
A more mathematical way to ask the question would be:

> Assuming that the runtimes of A and B follow a normal distribution with
> means <var>μ<sub>A</sub></var> and <var>μ<sub>B</sub></var> respectively,
> and variance <var>σ<sup>2</sup></var>,
> is it the case that <var>μ<sub>A</sub></var> = <var>μ<sub>B</sub></var>?

Note the assumptions here.
We assume that the runtime follows a normal distribution --
which might not always be a valid assumption!
For runtime in particular, a normal distribution can be a bad approximation,
because runtimes are always positive,
and because most sources of noise are biased towards increasing the observed time,
so the distribution should not be symmetric.
There is a good reason to opt for a normal distribution though:
it has been well-studied and it is easy to work with.
If the variance is small with respect to the runtime,
the approximation can be acceptable.
Furthermore, we assume that the variance is the same for both programs.

Under the assumption that
<var>μ<sub>A</sub></var> = <var>μ<sub>B</sub></var>,
