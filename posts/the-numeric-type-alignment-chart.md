---
title: A float walks into a gradual type system
date: 2025-02-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: I am building
---

I am building a new configuration language: [RCL][rcl-lang].
It’s a gradually typed superset of json
that extends json into a simple functional language
that enables abstraction and reuse.
Its main purpose is to generate json, yaml, and toml files,
but it makes a pretty good json query tool too.
Think jq, but without having to ask an LLM to write the query for you.
While RCL supported integers early on,
it was missing one piece to deliver on the json superset promise:
floats — numbers that contain a decimal point.
Adding floats to RCL was tough,
because several guiding principles are in conflict.
In this post we will explore the trade-offs involved.

## Json semantics

As a json superset,
RCL inherits and extends json’s number syntax.
The json specification is a _lexical_ one:
it prescribes what strings are valid json documents,
and it names the syntactic elements,
but it does not prescribe their _semantics_.
It’s up to the application to interpret the document.

In some cases,
all sane applications agree on the semantics.
For instance, `"?"`, `"\u003f"`, and `"\u003F"` all represent the same string.
For numbers, it’s not that clear.
Python’s json module parses `1` and `1.0` as different values
(an int and float respectively),
while in Javascript the two are indistinguishable.
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

[R<!---->C<!---->L features a gradual type system.][types]
The goal of the type system is to prevent bugs,
and to make code more self-documenting.
Because decimal points matter,
it would be useful to distinguish between ints and floats
in the type system.

Say we’re configuring `CPUSchedulingPriority` for a systemd service.
It is not obvious from the name that the value has to be integral.
If we could type the field as `Int`, rather than just `Number`,
that moves one constraint out of the documentation,
and into the code where it can be mechanically enforced.
On the other hand,
valid scheduling priorities range from 1 through 99,
and a mere `Int` does not encode _that_.

Where is the boundary between what is useful to model in the type system,
and what should just be an assertion?
If there is an integer type,
should there also be an unsigned integer type?
Types for integers of various sizes?
Full-blown refinement types?
My feeling when I started writing this post
was that a distinction between int and float would be useful,
but the more I think about it,
the less I am convinced that it’s worth the cost.
Which cost?
Let’s dive into the trade-offs.

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
many configuration schemas distinguish between integers and floats,
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
before realizing that equality doesn’t work as you assumed.

**Referential transparency.**<br>
If two values are equal,
then we should be able to substitute one for the other
and get the same result.
Suppose this is well-typed:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">A</span> = x;
<span class="kw">let</span> b: <span class="dt">B</span> = y;
</code></pre>

Then if `x == y`, the following should be well-typed too:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">A</span> = y;
<span class="kw">let</span> b: <span class="dt">B</span> = x;
</code></pre>

This substitution property is so fundamental,
that we rarely realize that we assumed it implicitly.
This is why a language without referential transparency
becomes difficult to reason about.

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
<span class="kw">let</span> a2: <span class="dt">Int</span> = <span class="dv">1.0</span>;
<span class="kw">let</span> b2: <span class="dt">Float</span> = <span class="dv">1</span>;
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
that should be readable even by people who haven’t seen it before.
It should be obvious and unsurprising,
so 1.0 can’t be an int.

And so the wishlist items are incompatible.
One of them has to go.

## Exploring the design space

The design space then,
is to pick which one goes.

**We could give up on the separate integer type.**<br>
There would be a single numeric type: `Number`.
This is what Typescript does.
This is the easiest way forward,
but I would find it a shame to lose the distinction between `Int` and `Float`.

**We could give up on allowing all values to be compared.**<br>
We don’t _need_ to assign a truth value to `1 == 1.0`,
we can say that it’s an error to even attempt the comparison!
This is what statically typed languages such as Rust and Haskell do.
In RCL though,
we can’t do this without throwing out sets and dicts with non-string keys.

**We could give up on referential transparency.**<br>
Value equality would be defined in a way that is incompatible with the type system.
This makes it hard to reason about general statements,
but that is rarely a problem in practice.
We could make 1 and 1.0 equal,
yet make it a type error to use 1.0 as int.
This is what Cue and Python + Mypy do.
It is perhaps the most practical way forward
that preserves the separate integer type,
but somehow it feels deeply unsatisfactory to me to violate referential transparency.
It should be possible to _reason_ about RCL,
and the principle that equal values are interchangeable is so fundamental,
that it’s hard to foresee the full impact of breaking that expectation.

**We could give up on numeric equality.**<br>
In any sane language, it’s hardly surprising that `1 != "1"`.
Couldn’t it be acceptable then,
that `1 != 1.0`?
Erlang’s `=:=` operator has this behavior,
though Erlang also features the regular `==` which does respect numeric equality.
I considered exploring this option with RCL.
It goes against the “obvious and unsurprising” goal,
but it is a coherent choice,
and uniform rules lead to a simpler language in the end
— [though maybe not easier][simple-easy].

## Conclusion

So, how should RCL handle floats?
I pondered this question for half a year,
and I went through many iterations of this post before
discovering the view of the design space that I presented here.
I was leaning towards giving up on numeric equality,
but as I wrote this post,
I changed my mind,
and I’ll go for a single number type instead.
Part of the reason is that `Int` and `Float` are somewhat arbitrary.
What motivates drawing the boundary there,
vs. adding unsigned integers?
Another reason is that it’s just simpler.
Simpler to document and explain to new users,
but definitely also to implement!
Scope creep is the enemy of shipping.
And it’s not like the decision is set in stone.
As of yet RCL makes no stability promise,
I can experiment.
Implement it,
get a feel for it,
and if I don’t like it,
I can just change the implementation!
With this decision out of the way,
I can finally return to coding,
and deliver on the json superset promise.

Now the next dilemma
— should the type be called `Number`, or `Num`?

## Thoughts

Referential transparency would also be broken if preserve insertion order on
dicts. You can turn the keys into a list,
so `f = d => [for k, _ in d: k]` can have different outputs for equal inputs.

When implementing, in 3 places ints mattered: `std.range`,
list indexing, and the `width` field in the `rcl build` schema.
A shame, but not that bad.

Verbosity matters.

A number type is okay for the same reason that runtime type checks are:
runtime errors are static errors.

[rcl-lang]:    https://rcl-lang.org
[types]:       /2024/a-type-system-for-rcl-part-1-introduction
[types-ii]:    /2024/a-type-system-for-rcl-part-2-the-type-system
[simple-easy]: https://www.infoq.com/presentations/Simple-Made-Easy/
