---
title: A type system for RCL, part 2: The type system
header: A type system for RCL
subheader: The type system
part: 2
lang: en-US
date: 2024-04-29
synopsis: TODO
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
The type system is a work in progress
— I plan to still add record types and type aliases,
and one thing I haven’t yet figured out
is how to enable importing types from files.
I don’t expect that these will fundamentally change the type system,
so let’s look at what we have so far.

## Strong types

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

```rcl
let a: Int = 0;
let b: Union[Int, String] = 0;
let c: Any = 0;
```

Collections are another example.
The runtime representation of a collection
does not depend on the element type,
so all of these are fine:

```rcl
let a: List[Void] = [];
let b: List[Int] = [];
let c: List[Union[Int, Bool]] = [0, false];
let d: List[Any] = [0, false];
```

That many types can describe the same value
matters especially for record types.
While they are not yet implemented,
this code will have to be well-typed:

```rcl
type Widget = { id: Int };
let a: Widget = { id = 42 };
let b: Dict[String, Int] = { id = 42 };
```

We can think of types as the set of values that the type allows.
That a value can have multiple types,
means that these sets are not disjoint.
That in turn means that we can order them by the subset relation,
so we have a partial order on types.*
Which brings us to the next topic:
types form a _lattice_.

\* The subset relation is not _quite_ the order on types,
for reasons we’ll see below.

## The type lattice

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
