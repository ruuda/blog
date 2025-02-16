---
title: The numeric type alignment chart
date: 2025-02-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: I am building
---

I am building a new configuration language, [RCL][rcl-lang].
It’s a superset of json
that extends json into a simple functional language
that enables abstraction and reuse.
Its main purpose is to generate json, yaml, and toml files,
but it makes a pretty good json query tool too.
Think jq, but without having to ask an LLM to write the query for you.
While RCL supported integers early on,
it was missing one piece to deliver on the json superset promise:
floats — numbers that contain a decimal point or exponent.
Adding floats to RCL was tough,
because of several conflicting principles.
In this post we will explore that trade-off.
The primary question we’ll have to answer is:
what is the relation between 1 and 1.0?

## Json semantics

As a json superset,
RCL inherits and extends json’s number syntax.
The json specification is a _lexical_ one:
it prescribes what strings are valid json documents,
but it does not prescribe the _semantics_ of those documents.
It’s up to the application to interpret the document.

In some cases,
all sane applications agree on the semantics.
For example,
whitespace is insignificant,
and `"?"`, `"\u003f"`, and `"\u003F"` all represent the same string.
For numbers, it’s not that clear.
For example,
Python’s json module parses `1` and `1.0` as different values
(an int and string respectively),
while in JavaScript the two are indistinguishable.
By default Python parses `1.0` and `1.00` as the same value,
but it can be configured to preserve the distinction.
And many deserializers reject numbers with a decimal point
when they expect an integer,
even if the fractional part is zero.

Because RCL aims to generate configuration
for any application that accepts json, yaml, or toml,
it can’t assume that the presence or absence of a decimal point is irrelevant.
It should never silently insert or remove decimal points.

## Types

[R<!---->C<!---->L is gradually typed.][types]
The goal of the type system is to prevent bugs,
and to make code more self-documenting.
Because decimal points matter,
it would be useful to distinguish between ints and floats.

## Wishlist

R<!---->C<!---->L aims to be a _reasonable_ configuration language.
It should make reasonable choices;
its behavior should be unsurprising.
But also,
it should be easy for humans to reason about RCL programs.
This is easier when there are rules that apply universally,
and harder when there are ad-hoc exceptions to such rules.
With that in mind,
this is what I would like to see from numbers in RCL:

**A separate integer type.**<br>
While we could go the Javascript route of having only “numbers”,
many configuration formats distinguish between integers and floats,
so it is useful for RCL to be able to express that difference
in the type system.

**All values can be compared.**<br>
Equality is defined between any two values,
even when they have different types.
The `==` operator test for equality explicitly,
but sets and dictionaries rely on equality as well.
This requirement is a consequence of generalizing json:
json allows heterogeneous lists,
so if we add sets and allow arbitrary dict keys,
those should be allowed to be heterogeneous too.

**Equality should respect numeric equality.**<br>
Users expect `1 == 1.0` to be true.
When it’s false,
that is one of those footguns where you debug for hours
before realizing that equality doesn’t work as you expected.

**Referential transparency.**<br>
If two values are equal,
then we should be able to substitute one for the other
and get the same result.
Suppose we have this:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">A</span> = x;
<span class="kw">let</span> b: <span class="dt">B</span> = y;
</code></pre>

Then if `x == y`, the following should be well-typed:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">A</span> = y;
<span class="kw">let</span> b: <span class="dt">B</span> = x;
</code></pre>

Without referential transparency,
a language becomes really hard to reason about.

<!--
(Note that this does not imply that `A = B`,
because in RCL [values do not have unique types][types-ii].)
-->

## The clash

If we have separate types for `Int` and `Float`,
then referential transparency combined with numeric equality
leads to the following conclusion:

<pre><code class="sourceCode"><span class="co">// This is all fine and normal.</span>
<span class="kw">let</span> a1: <span class="dt">Int</span> = <span class="dv">1</span>;
<span class="kw">let</span> b1: <span class="dt">Float</span> = <span class="dv">1.0</span>;

<span class="co">// This is true.</span>
a1 == b1

<span class="co">// Therefore, the following must be well-typed!</span>
<span class="kw">let</span> a1: <span class="dt">Int</span> = <span class="dv">1.0</span>;
<span class="kw">let</span> b1: <span class="dt">Float</span> = <span class="dv">1</span>;
</code></pre>

I find this bizarre and counter-productive.
While 1.0 is numerically an integer,
in programming, integers are numbers without decimal point.
That’s what ‘integer’ means in configuration formats,
and deviating from that common meaning would defeat the point
of making a distinction between ints and floats.

It is possible to build a coherent type system
that satisfies all my wishlist items,
for example with refinement types,
but I want RCL to be simple and boring.
It’s not a research language
intended to explore the cutting edge of configuration systems,
it’s a practical tool
for people without a background in advanced functional programming.
It should be obvious and unsurprising,
so 1.0 can’t be an int.

And so the wishlist items are incompatible.
One of them has to go.
The design space then,
is to pick which one goes.

**We could give up on the separate integer type.**
There would be a single numeric type: `Number`.
This is what TypeScript does.
This is the easiest way forward,
but I would find it a shame to lose the distinction between `Int` and `Float`.

**We could give up on allowing all values to be compared.**
We don’t _need_ to assign a truth value to `1 == 1.0`,
we can say that it’s an error to even attempt the comparison!
This is what statically typed languages such as Rust and Haskell do.
In RCL though,
we can’t do this without throwing out sets and dicts with non-string keys.

**We could give up on referential transparency.**
Value equality would be defined in a way that is incompatible with the type system.
This makes it hard to reason about general statements,
but that is rarely a problem in practice.
We could make 1 and 1.0 equal,
yet make it a type error to use 1.0 as int.
This is what Cue and Python + Mypy do.
It is perhaps the most practical way forward
that preserves the separate integer type,
but somehow it feels deeply unsatisfactory to me to violate referential transparency.
It’s a core principle that it should be possible to _reason_ about RCL,
and the expectation that equal values are interchangeable is so fundamental,
that it’s hard to foresee the full impact when we break that expectation.

**We could give up on numeric equality.**
In any sane language, it’s hardly surprising that `1 != "1"`.
Couldn’t it be acceptable then,
that `1 != 1.0`?
This is what I would like to explore with RCL.
It’s not the most obvious and unsurprising choice,
but it is a coherent one,
and my gut feeling is that
uniform rules lead to a simpler
— [though maybe not easier][simple-easy]
— language in the end.

To limit the footguniness of this choice,
we can at least disallow comparison operators (`<`, `<=`, `>`, `>=`)
between values of different types.
`1 < 2`, `"a" < "b"`, and `0.5 < 2.0` are all true,
but `0.5 < 2` is an error.
<!-- TODO: Mention List.sort. -->

## Precision

Wat do if input exceeds the range?
I think it is fair to assume that `1.0` and `1.00` are interchangeable
— at some point you have to be pragmatic,
and applications that care about the exact number format
tend to encode numbers as strings anyway.

[rcl-lang]:    https://rcl-lang.org
[types]:       /2024/a-type-system-for-rcl-part-1-introduction
[types-ii]:    /2024/a-type-system-for-rcl-part-2-the-type-system
[simple-easy]: https://www.infoq.com/presentations/Simple-Made-Easy/
