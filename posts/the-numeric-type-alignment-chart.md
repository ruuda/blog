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

## Design space

**We could give up on the separate integer type**.
There would be a single numeric type: `Number`.
This is what TypeScript does.

**We could give up on allowing all values to be compared.**
We don’t _need_ to assign a truth value to `1 == 1.0`,
we can say that it’s an error to even attempt the comparison!
This is what statically typed languages such as Rust and Haskell do.

**We could give up on referential transparency.**
Value equality would be defined in a way that is incompatible with the type system.
This makes it hard to reason about generic statements,
but it’s rarely a problem in practice.
We could make 1 and 1.0 equal,
yet make it a type error to use 1.0 as int.
This is what Cue and Python/Mypy do.

**We could give up on numeric equality.**
In any sane language, it’s hardly surprising that `1 != "1"`.
Couldn’t it be acceptable then,
that `1 != 1.0`?
This is what I plan to do for RCL.

## The Chart

The following alignment chart summarizes this challenge:


<!-- TODO: There should be some axis about whether different types of equality
are allowed to co-exist and how they interact with types. -->

<div style="overflow-x: auto">
<div style="overflow: hidden; width: fit-content">
<table style="border-spacing: 1em; margin: 0 -1em 1em -1em; min-width: 30em">
<thead>
<tr>
  <td></td>
  <td>
    <strong>Value purist</strong>
    <br>Value identity must respect numeric equality.
  </td>
  <td>
    <strong>Value neutral</strong>
    <br>A decimal point is part of a number’s identity.
  </td>
  <td>
    <strong>Value rebel</strong>
    <br>Formatting is part of a number’s identity.</td>
  </td>
</tr>
</thead>
<tbody>
<tr>
  <td>
    <strong>Type purist</strong>
    <br>A number is either an int or a float.
  </td>
  <td><code>1 == 1.0</code> and <code>{1, 1.0}</code> are type errors.</td>
  <td><code>1 ≠ 1.0</code>,<br>just like<br><code>1 ≠ "1"</code>.</td>
  <td><code>1.0 ≠ 1.00</code>,<br>just like<br><code>"1.0" ≠ "1.00"</code>.</td>
</tr>
<tr>
  <td>
    <strong>Type neutral</strong>
    <br>Int is a subtype of float.
  </td>
  <td><code>{1, 1.0}</code> has one element.<br><code>1.0</code> is an int.</td>
  <td><code>{1, 1.0}</code> has two elements.<br><code>1</code> is an int.</td>
  <td>Ints and floats are strings that match regexes.</td>
</tr>
<tr>
  <td>
    <strong>Type rebel</strong>
    <br>All numbers are floats.
  </td>
  <td><code>{1, 1.0}</code> has one element.<br><code>1</code> is a float.</td>
  <td><code>{1, 1.0}</code> has two elements.<br><code>1</code> is a float.</td>
  <td>Numbers are strings that match a regex.</td>
</tr>
</tbody>
</table>
</div>
</div>

Before we dive in to what this means,
let’s look at where the challenge comes from.

## Alternative

An alternative chart might be:

 * Equality purist: There is one kind of equality.
   Value identity must respect numeric equality.
   Values that have disjoint types cannot be equal.
 * Equality neutral: Values can have non-identifying properties.
   Value identity must respect numeric equality.
   For example `1.0` and `1.00` are different but equal,
   just as `{ a = 1, b = 2}` and `{b = 2, a = 1}` are different but equal.
   Values that are different can be equal.
   `1.0 = 1.00`, `1 = 1.0`.
 * Equality rebel (or is this purist?):
   A decimal point is part of a number’s identity.
   `1 ≠ 1.0`,
   just like `1 ≠ "1"`.
 * Equality rebel:
   Numbers can be equal to strings.

There is precedent that values that look different are equal (dicts),
and although RCL normalizes them currently,
it should preserve insertion order instead.
There is precedent that values with different types can be equal,
e.g. `[]: List[Bool]` and `[]: List[String]` are equal.
Though that check should be a type error.

## Precision

Wat do if input exceeds the range?
I think it is fair to assume that `1.0` and `1.00` are interchangeable
— at some point you have to be pragmatic,
and applications that care about the exact number format
tend to encode numbers as strings anyway.

[rcl-lang]: https://rcl-lang.org
[types]:    /2024/a-type-system-for-rcl-part-1-introduction
[types-ii]: /2024/a-type-system-for-rcl-part-2-the-type-system
