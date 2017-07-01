---
title: On benchmarking
date: 2017-07-01
minutes: ??
synopsis: ??
run-in: Measuring performance is hard
---

Measuring performance is hard.
Interpreting the data may be even harder.
I recently stumbled upon a Reddit thread
where people attempted to compare performance of a few programs.
The results went something like this:

Program  Runtime (ms)
-------  ------------
A        6360
B        5116

Quickly others joined posting the results for their systems.
At some point somebody questioned the measurement setup.
But at no point did somebody question the conclusion.
Clearly program B is the faster one, right?

Not so fast -- from this data it is impossible to tell.
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
it is not even clear what the conclusion should be.
Fortunately, statistics provides an answer here.
