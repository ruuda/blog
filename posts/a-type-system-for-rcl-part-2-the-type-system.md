---
title: A type system for RCL, part 2: The type system
header: A type system for RCL
subheader: The type system
part: 2
lang: en-US
date: 2024-04-29
synopsis: TODO
extra-glyphs: Null Int Bool String Any Void List[]
---

<span class="run-in">I am [building][rcl-intro]</span> a new configuration language:
[RCL][rcl-lang].
It extends json into a simple functional language
that enables abstraction and reuse.
In this series we take a closer look at the type system,
because I think it is interesting,
but also because I hope that exposure to the wider PL community
can help me to validate some of the ideas,
or find problems with them.

[rcl-intro]: /2024/a-reasonable-configuration-language
[rcl-lang]:  https://rcl-lang.org/

 * [Part <abbr>I</abbr>: Introduction][part1]
 * [Part <abbr>II</abbr>: The type system][part2] (this post)
 * [Part <abbr>III</abbr>: The typechecker][part3]

In part <abbr>I</abbr> we looked at what I want from a type system for RCL.
In this part we’ll look at the type system so far.
The type system is a work in progress.
I plan to still add record types and type aliases,
and one thing I haven’t yet figured out
is how to enable importing types from files.
I don’t expect that these will fundamentally change the type system,
so let’s look at what we have so far.

## Strong typing

Even before the addition of the typechecker,
RCL was strongly typed,
in the sense that it does not convert between data types implicitly.
For example,
the condition in an if-else expression really needs to be a boolean.
When the condition is e.g. an empty list,
RCL does not try to guess what the programmer might have meant,
it fails with a type error instead.
Before the addition of the typechecker,
this would have been a runtime type error.
Now, in many cases it is a static type error,
though there are still cases where the the typechecker
has to insert a runtime check.
We’ll see why below.

## Basics

Two major choices affect RCL’s type system:

 1. **Types constrain values, but values don’t have unique types.**
    The type system specifies whether a value “fits” a type,
    but the same value can fit multiple types.
    Variables that have different types can refer to values that are equal.
 2. **Type inference is forward-only, and mostly bottom-up.**
    The typechecker assigns a concrete type to every bound variable;
    there is no constraint propagation that makes type information flow backwards.
    Although the typechecker always infers _some_ type,
    the inferred type can be less precise
    than what backwards inference could find.
    Forward-only inference keeps the typechecker fast,
    and tractable for humans.
    After all, RCL tries to be a _reasonable_ configuration language:
    humans should be able to reason about whether a program is well-typed.

## Types constrain values

What does it mean that the same value can fit multiple types?
It means that all of these are well-typed:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">Int</span> = <span class="dv">0</span>;
<span class="kw">let</span> b: <span class="dt">Any</span> = <span class="dv">0</span>;
<span class="kw">let</span> c: <span class="dt">Union</span>[<span class="dt">Int</span>, <span class="dt">String</span>] = <span class="dv">0</span>;
</code></pre>

Collections are another example:
The runtime representation of a collection
does not depend on the element type,
so all of these are fine:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">List</span>[<span class="dt">Void</span>] = [];
<span class="kw">let</span> b: <span class="dt">List</span>[<span class="dt">Int</span>] = [];
<span class="kw">let</span> c: <span class="dt">List</span>[<span class="dt">Union</span>[<span class="dt">Int</span>, <span class="dt">Bool</span>]] = [<span class="dv">0</span>, <span class="kw">false</span>];
<span class="kw">let</span> d: <span class="dt">List</span>[<span class="dt">Any</span>] = [<span class="dv">0</span>, <span class="kw">false</span>];
</code></pre>

That many types can describe the same value
matters especially for record types.
While they are not yet implemented,
this code would have to be well-typed:

<pre><code class="sourceCode"><span class="kw">type</span> <span class="dt">Widget</span> = { id: <span class="dt">Int</span> };
<span class="kw">let</span> a: <span class="dt">Widget</span> = { <span class="n">id</span> = <span class="dv">42</span> };
<span class="co">// Recall that { a = b } is record notation for the dict { "a": b }.</span>
<span class="kw">let</span> b: <span class="dt">Dict</span>[<span class="dt">String</span>, <span class="dt">Int</span>] = { <span class="n">id</span> = <span class="dv">42</span> };
</code></pre>

We can think of types as the set of values that fit that type.
That a value can have multiple types,
means that these sets are not disjoint.
That in turn means that we can order them by the subset relation,
so we have a partial order on types.*
Which brings us to the next topic:
types form a _lattice_.

## The type lattice

A lattice — strictly speaking a _meet-semilattice_ in this case —
is a partially ordered set with an operation called _meet_
that returns the greatest lower bound of two elements.
Part of the lattice looks like this:

![A part of the type lattice.](/images/lattice.svg)

The bottom of the lattice is `Void`, the uninhabited type.
It corresponds to the empty set.
The top of the lattice is `Any`, the type that any value fits.
It corresponds to the set of all values.
In between we have primitive types like `Int` and `String`,
but also collection types like `List[Int]`,
a list of integers.

The partial order on types is the subtyping relationship.
If we view types as sets,
then the subset relationship is _almost_ the subtype relationship.
With the notation `T ≤ U` for “`T` is a subtype of `U`”,
and `t:` `T` for “`t` fits `T`”,
the subset-based relationship would be the following:

 * We say `T ≤ U` if for all `t:` `T` it holds that `t:` `U`.
 * We say `T` and `U` are unorderable
   if there exist `t:` `T` and `u:` `U`

TODO: Lattice itself.
TODO: It’s gradual, so we need `Any`.
TODO: Sloppy join = fast inference.
TODO: Ref [Cue][cue-lattice].

[cue-lattice]: https://cuelang.org/docs/concept/the-logic-of-cue/

## Static typing

R<!---->C<!---->L is statically typed,
in the sense that it can report type errors in unreachable code.
For example,
the following program fails with a type error:

<pre><code class="sourceCode"><span class="kw">let</span> string = <span class="st">"strings cannot be negated"</span>;
<span class="kw">if</span> <span class="kw">false</span>:
  <span class="co">// Error: Type mismatch. Expected Bool but found String.</span>
  <span class="kw">not</span> string
<span class="kw">else</span>
  <span class="kw">true</span>
</code></pre>

However,
although RCL enforces all type annotations,
it defers some type checks to runtime.
The following is fine:

<pre><code class="sourceCode"><span class="kw">let</span> string: <span class="dt">Any</span> = <span class="st">"strings cannot be negated"</span>;
<span class="kw">if</span> <span class="kw">false</span>: <span class="kw">not</span> string <span class="kw">else</span> <span class="kw">true</span>
</code></pre>

In general,
the typechecker can encounter three cases:

* It can prove that the program contains a type error.
  In this case it reports the error.
* It can prove that the program is well-typed.
  In this case we proceed to evaluation.
* It can’t rule out a type error, but it can’t prove it either.
  In this case it inserts a runtime type check.

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-the-typechecker

TODO: Write no HM, no constraints.
[Swift is slow][swift-slow]

[swift-slow]: https://danielchasehooper.com/posts/why-swift-is-slow/
