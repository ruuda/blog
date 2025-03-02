---
title: A float walks into a gradual type system
date: 2025-03-02
lang: en-US
minutes: 12
synopsis: In this post I explore the design space of adding floats to RCL, a gradually typed configuration language. With floats in place, RCL is finally a true json superset.
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
Furthermore, many deserializers reject numbers with a decimal point
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
My feeling when I started writing this post,
was that a distinction between int and float would be useful,
but the more I think about it,
the less I am convinced that it’s worth the cost.
Which cost?
Let’s dive into the trade-offs.

[types]:       /2024/a-type-system-for-rcl-part-1-introduction

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
so it is useful for RCL to be able to express that difference.
Moreover, some operations in RCL itself,
such as list indexing,
accept ints but not floats.

**All values can be compared.**<br>
Equality is defined between any two values,
even when they have different types.
The `==` operator tests for equality explicitly,
but sets and dictionaries rely on equality as well.
This requirement is a consequence of generalizing json:
json allows heterogeneous lists like `[1,` `"a"]`,
so if we add sets and allow arbitrary dict keys,
those should be allowed to be heterogeneous too.

**Equality should respect numeric equality.**<br>
People expect `1 == 1.0` to be true.
When it’s false,
that is one of those footguns where you debug for hours
before realizing that equality doesn’t work the way you assumed.

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
This is why violating referential transparency
makes a language more difficult to reason about.

<!--
(Note that this does not imply that `A = B`,
because in RCL [values do not have unique types][types-ii].)
[types-ii]:    /2024/a-type-system-for-rcl-part-2-the-type-system
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
but it would have to accept the example above.
While that would be interesting to explore,
I want RCL to be simple and boring.
It’s not a research language
intended to explore the cutting edge of configuration systems,
it’s a practical tool
that people who haven’t seen it before should be able to pick up quickly.
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
but somehow it feels deeply unsatisfactory to me.
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

[simple-easy]: https://www.infoq.com/presentations/Simple-Made-Easy/

## Decision

So, how should RCL handle floats?
I pondered this question for half a year,
and I went through many iterations of this post before
discovering the view of the design space that I presented here.
I was leaning towards giving up on numeric equality,
but as I wrote this post,
a single number type started to feel more and more like the right approach.
Part of the reason is that `Int` and `Float` are somewhat arbitrary.
What motivates drawing the boundary there,
vs. adding unsigned integers?
Another reason is that it’s simpler:
fewer concepts to document,
and explain to new users.
Scope creep is the enemy of shipping.
And it’s not like the decision is set in stone.
As of yet RCL makes no stability promise,
I can experiment.
Implement it,
get a feel for it,
and if I don’t like it,
I can just change it.

## Thoughts

As somebody who likes strong static type systems,
I have to admit that replacing `Int` with `Number` hurt a little.
There were three places — fewer than I expected! —
that previously were infallible by construction,
but now need a runtime check to ensure that a number is an integer.
For example, when indexing into a list.

Fortunately, runtime checks are not so bad in RCL.
In a general-purpose language where evaluation is separate from typechecking,
a runtime check makes the type system less effective at preventing crashes.
But the job of RCL’s type system is not to prevent runtime errors
— in a configuration language,
[runtime errors are static errors][static].

[static]: /2024/a-type-system-for-rcl-part-1-introduction#static-vs.-runtime

One thing that still bothers me slightly is verbosity.
Compared to the concise `Int`, `Number` is 100% longer!
This sounds like a superficial complaint,
and it is,
but style matters.
Nobody likes Java’s verbosity.
Many examples that previously fit on a single line,
don’t look so neat any more.
I considered calling the type `Num` rather than `Number`,
but that feels unnecessarily arcane for new users.
I want RCL to be boring and obvious,
but not to the point where that takes away the joy of using it.
Fortunately,
like the design of the type system,
nothing is set in stone.
I implemented `Number` for now,
let’s get a feel for it,
and if I don’t like it,
I can just change it later.

Finally,
maybe I’m weighing referential transparency too heavily.
I expect that even RCL
will violate the substitution property at some point.
For example,
I want to make dicts preserve their insertion order (like in Python),
which means that `a == b` no longer implies that `f(a) == f(b)`.
After all, `f` could iterate a dict and build a list from it.
Still, few exceptions are better than many.

## Conclusion

Adding floats to RCL was challenging.
Not because the implementation was hard,
but because I did not want to implement something ad-hoc.
I want RCL to have strong underlying principles
that make it possible to reason about its behavior,
and floats have to respect those.
In this post we explored the design space for that.
For now, I chose to go with a single numeric type.
The result of this journey is [RCL 0.8.0][v080],
the first version to fully deliver on the json superset promise.
[Give it a spin][rcl-lang],
and let me know what you think!

[rcl-lang]: https://rcl-lang.org
[v080]:     https://docs.ruuda.nl/rcl/changelog/#080
