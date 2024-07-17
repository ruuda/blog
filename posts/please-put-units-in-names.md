---
title: Please put units in names
break: put units
subheader: or use <span class="dlig">strong</span> types
date: 2022-03-20
lang: en-US
minutes: 4
synopsis: Using strong types, or putting units in names, is a small effort that can make a tremendous difference for code readability.
run-in: There is one
---

There is one code readability trap
that is easy to avoid once you are aware of it,
yet the trap is pervasive: omitting units.
Consider the following three snippets in Python, Java, and Haskell:

```python
time.sleep(300)
```
```java
Thread.sleep(300)
```
```haskell
threadDelay 300
```

How long do these programs sleep for?
The Python program sleeps for five minutes,
the Java program sleeps for 0.3 seconds,
and the Haskell program sleeps for 0.3 milliseconds.

How can you tell this from the code?
You can’t.
You just have to know by heart that `time.sleep` takes seconds,
while `threadDelay` takes microseconds.
If you look it up often enough,
eventually that knowledge will stick,
but how can we keep the code readable
even for people who haven’t encountered `time.sleep` before?

Option 1: put the unit in the name
----------------------------------

Instead of this:
```python
def frobnicate(timeout: int) -> None:
    ...

frobnicate(300)
```

Do this:
```python
def frobnicate(*, timeout_seconds: int) -> None:
    # The * forces the caller to use named arguments
    # for all arguments after the *.
    ...

frobnicate(timeout_seconds=300)
```

In the first case, we can’t even tell at the call site that 300 is a timeout,
but even if we knew that, it’s a timeout of 300 what? Milliseconds? Seconds?
Martian days? In contrast, the second call site is completely self-explanatory.

Using named arguments is nice for languages that support it,
but this is not always a possibility.
Even in Python, where `time.sleep` is defined with a single argument named `secs`,
we can’t call `sleep(secs=300)` due to implementation reasons.
In that case, we can give the value a name instead.

Instead of this:

```python
time.sleep(300)
```

Do this:

```python
sleep_seconds = 300
time.sleep(sleep_seconds)
```

Now the code is unambiguous,
and readable without having to consult the documentation.

Option 2: use strong types
--------------------------

An alternative to putting the unit in the name,
is to use stronger types than integers or floats.
For example, we might use a duration type.

Instead of this:
```python
def frobnicate(timeout: int) -> None:
    ...

frobnicate(300)
```

Do this:
```python
def frobnicate(timeout: timedelta) -> None:
    ...

timeout = timedelta(seconds=300)
frobnicate(timeout)
```

For a given floating-point number,
you need to be told out of band what the unit is to be able to interpret it.
If you are lucky this information is in the variable or argument name,
but if you are unlucky it’s only specified in the documentation
— or not specified at all.
But for a `timedelta` value,
there is no ambiguity about how to interpret it,
this is part of the type.
This also removes the ambiguity from the code.

Scope
-----

The advice to use strong types or to put units in names
is not limited to variables and function arguments,
it’s applicable to <abbr>API</abbr>&thinsp;s,
[metric names](https://prometheus.io/docs/practices/naming/#metric-names),
serialization formats, configuration files, command-line flags, etc.
And although duration values are the most common case,
this advice is not limited to those either,
it also applies to monetary amounts, lengths, data sizes, etc.

For example, don’t return this:
```json
{
   "error_code": "E429",
   "error_message": "Rate limit exceeded",
   "retry_after": 100,
}
```

Return this instead:
```json
{
   "error_code": "E429",
   "error_message": "Rate limit exceeded",
   "retry_after_seconds": 100,
}
```

Don’t design your config file like this:
```
request_timeout = 10
```

Accept one of these instead:
```
request_timeout = 10s
request_timeout_seconds = 10
```

And don’t design your CLI accounting app like this:
```
show-transactions --minimum-amount 32
```

Accept one of these instead:
```
show-transactions --minimum-amount-eur 32
show-transactions --minimum-amount "32 EUR"
```
