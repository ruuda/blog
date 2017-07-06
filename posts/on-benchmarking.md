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

Presentation
------------

We have seen that one measurement per scenario is too little;
with only one sample we cannot estimate the accuracy of the measurement.
So we measure multiple times.
The issue then, is one of presentation:
how do we distil the raw data into a few *useful* numbers?

I started this post by arguing that a single number
is not sufficient to present measurement results.
The minimum we can distil the data to is two numbers.
Which two?
One approach is to summarise the data in one number,
and to also give the spread around that number.
For example, we could take the mean and the standard deviation,
or the median and the width of a 95% confidence interval.
The particular measure of deviation is not very important.
Its main role is to quantify the accuracy of the summary.

What is the best way to summarise a set of benchmark measurements in one number?
Andrei Alexandrescu [argues][minimum] that for optimisation purposes,
it is the minimum:
it is the best-case performance that you might achieve.
While I agree that the minimum is a good summary
if best-case performance is what you are after,
the best case is not always that interesting.
When optimising a small part of a program in isolation,
then yes, best-case performance is the thing to measure:
everything else is noise caused by other components,
cluttering the thing you are trying to assess.
But if your application uses a cache that has an 80% hit rate in production,
is it fair to discard those 20% of slower results?
It depends on what you want to measure.

* **When optimising a component in isolation,
  take the minimum.**
  If you are timing a deterministic computation,
  anything other than the minimum is external interference.
  The minimum might not be representative of a typical run.
* **When throughput is what you are after,
  use the mean.**
  A mean is like a total.
  After all it is just the sum of all values scaled by a constant.
  An interpretation of the mean is
  “if you run the program <var>x</var> times,
  where <var>x</var> is large,
  the expected runtime is <var>x</var> times the mean.”
  For a single run however, the mean can be misleading,
  because it includes amortized outliers.
  The mean is not necessarily representative of a typical run.
* **If you care about latency, use the median.**
  An interpretation of the median is
  “if you run the program once,
  it will be faster than the median with 50% probability.”
  The median is less sensitive to outliers than the mean,
  and may be a much better reflection of a typical run.
  However, the median cannot be used to assess throughput,
  because it fails to account for the outliers.

The minimum and median are special cases of quantiles:
the 0-quantile and 0.5-quantile.
Other quantiles can be useful too.
If you don’t care about latency of a typical run,
but rather about worst-case performance,
the 0.9 or even 0.99-quantile may be a good summary.
(The maximum is rarely a good idea,
because a program can experience
an unbounded amount of slowdown in unlucky cases,
whereas there is a limit to how quickly a program can run.)
A caveat of extreme quantiles is that you need sufficient data
to be able to compute them reliably:
if you have less than 100 data points,
the 0.01-quantile must be extrapolated from the data, rather than computed.
And even if you have that many data points,
extreme quantiles are sensitive to noise.
The 0.5-quantile on the other hand
is usually very stable and insensitive to outliers
even for few data points.

Make a plot here, of the distribution of the 0.9-quantile and 0.5-quantile?

A statistical test
------------------

Let’s say we set out to answer the question
“is program B faster than program A?”
The proper way to answer such a question is with a statistical test.
A statistical test always has the same form.
First we state the null hypothesis,
the claim that we assume to be true without sufficient evidence of the contrary.
Then we measure.
If the measured data is unlikely
under the assumption that the null hypothesis is true,
we reject the null hypothesis.
Concretely, we might go about that as follows:

Assume that the runtimes of A and B follow a normal distribution with
means <var>μ<sub>A</sub></var> and <var>μ<sub>B</sub></var>
and variance <var>σ<sub>A</sub><sup>2</sup></var>
and <var>σ<sub>B</sub><sup>2</sup></var> respectively.
We take as null hypothesis <var>μ<sub>A</sub></var> = <var>μ<sub>B</sub></var>,
i.e. neither program is faster than the other.
By extension, the alternative hypothesis is
<var>μ<sub>A</sub></var> ≠ <var>μ<sub>B</sub></var>.
The alternative hypothesis does not favour any program,
it is simply the negation of the null hypothesis.

Note that we assume that the runtime follows a normal distribution --
which might not be a valid assumption!
Still, there is a compelling reason to opt for a normal distribution:
it has been well-studied and it is easy to work with.
If the variance is small with respect to the runtime,
the approximation can be acceptable.

Under the above assumptions,
we can apply Welch’s t-test to measured data to obtain a _p-value_:
the probability of an observation at least as extreme as the actual data,
under the assumption that the null hypothesis is true.
If that probability is small, we reject the null hypothesis,
and conclude that one program is significantly faster than the other.
How small? That is for you to decide, _before_ you do any measurements.
But please think about it,
and don’t blindly take 0.05 because you have seen that value [elsewhere][nova].
Especially when there is no shortage of data
because measurements can be automated,
we can have much higher standards.

The math behind the Welch’s t-test is beyond the scope of this post,
but fortunately applying the test is easy.
Many implementations exist, for instance in SciPy or R:

```r
# Generate random data; actual measurements go here when available.
runtimesA <- rnorm(mean = 5800, sd = 600, n = 10)
runtimesB <- rnorm(mean = 5400, sd = 500, n = 10)
t.test(runtimesA, runtimesB)
# Output:
#
#    Welch Two Sample t-test
#
# data:  runtimesA and runtimesB
# t = 0.81516, df = 17.69, p-value = 0.4258
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -327.9776  742.9935
# sample estimates:
# mean of x mean of y
#  5733.310  5525.802
```

Even though B was actually faster by construction,
in this case we cannot conclude from the data
that either program was significantly faster.
To gain more confidence,
we could increase the number of samples,
or try to reduce the variance.


Presentation
------------

Something something significant digits tables.

Minimum vs mean (throughput, continuous) vs median (latency, one-off).

[nova]:    https://www.xkcd.com/1132/
[minimum]: https://youtu.be/vrfYLlR8X8k?t=15m15s
