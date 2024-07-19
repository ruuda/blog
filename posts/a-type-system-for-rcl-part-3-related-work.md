---
title: A type system for RCL, part 3: Related work
header: A type system for RCL
subheader: Related work
part: 3
lang: en-US
minutes: 9
date: 2024-07-18
synopsis: I am adding a type system to RCL, my configuration language. The type system is based on ideas from other systems. In this post I highlight prior work, and I contrast RCL’s type system with that of other configuration languages.
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
 * [Part <abbr>II</abbr>: The type system][part2]
 * [Part <abbr>III</abbr>: Related work][part3] (this post)
 * [Part <abbr>IV</abbr>: The typechecker][part4]

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/a-type-system-for-rcl-part-4-the-typechecker

In part one we looked at what I want from a type system for RCL,
and in part two we saw how the type system tries to achieve those goals.
The resulting type system is not a completely new invention,
it is based explicitly and implicitly
on other languages that I’ve worked with,
and ideas that I have been exposed to.
R<!---->C<!---->L is not the first typed json superset,
nor is it the first typed configuration language.
In this post I want to take a break from RCL itself
and acknowledge some of the work that inspired RCL,
and contrast RCL with alternatives in this space.

## Python and Mypy

[Mypy][mypy] is the most mature typechecker for Python,
and it is the gradual type system that I have most experience with.
I’ve been using it since Python 2,
back when annotations were still in comments.
Mypy is one of the main influences for RCL’s type system.
The type system differs from RCL in important ways
(in particular, RCL enforces annotations at runtime, unlike Python),
but the look and feel of RCL intentionally resembles Python,
and the square bracket syntax for generic types
is inspired by Python and Scala.

[mypy]: https://www.mypy-lang.org/

## Monotone frameworks

The idea of using a join-semilattice for type inference
is based on monotone frameworks,
an approach for inferring types for dynamically typed programs.
I learned about this from Jurriaan Hage
in a course taught at Utrecht University.
In the lab assignment at the time
I built a lattice-based typechecker for Lua.
Monotone frameworks
propagate lattice elements over a control flow graph.
The control flow graph is not used in RCL’s typechecker,
and less relevant to a functional language with immutable bindings anyway,
but the lattice part is a great foundation for a type system.
I can’t find an obvious authoritative source to link to,
[but I found this slide deck][jhage-uu] that treats the topic.

[jhage-uu]: https://www.macs.hw.ac.uk/~jh2054/downloads/pythonbalancing-talk.pdf

## Cue

[Cue][cue] is another typed configuration language,
and its types [form a lattice as well][cue-lattice].
In fact, the lattice in Cue is very similar to the one in RCL.
Cue goes a step further by merging the type and value universes,
and I really like how that approach can be used
both for constraining values and reducing boilerplate.
Though the approach is really elegant and powerful,
it requires a bit of a mental leap.
For RCL,
I wanted something that is completely obvious and boring
to somebody familiar with more traditional imperative languages;
something that you don’t need to learn,
because you already know it,
even when you see it for the first time.
As a language,
RCL is more similar to [Jsonnet][jsonnet]
(very similar actually),
though Jsonnet does not have static types.

[cue]:         https://cuelang.org/
[cue-lattice]: https://cuelang.org/docs/concept/the-logic-of-cue/
[jsonnet]:     https://jsonnet.org/

## TypeScript

Just as Mypy adds types to Python,
[TypeScript][typescript] adds types to JavaScript.
It _should_ be very relevant for me,
because TypeScript is a superset of json,
so like RCL its type system has to be able to describe arbitrary json data.
TypeScript’s type system solves a harder problem than RCL,
because it has to type an imperative language with mutable objects.
I try hard to avoid the NPM ecosystem,
so I haven’t used TypeScript much except in a few small applications,
and I haven’t studied its type system in detail.

[typescript]: https://www.typescriptlang.org/

## PureScript

[PureScript][purescript] is a typed functional language similar to Haskell.
It compiles to and interoperates with JavaScript,
so it has good support for modelling JavaScript objects,
which correspond to dicts in RCL.
I’ve written two applications in it
([a plant watering tracker][sempervivum] and [a music player][musium]),
and I feel more comfortable with it than TypeScript.
So far I haven’t encountered the need for [row polymorphism][rowpoly].
I need to dive into this deeper when I get to adding record types to RCL,
but my feeling is that row types may be a bit too advanced.
I want RCL to be obvious to understand
even for people who don’t have a deep background in typed functional programming.
It’s okay if that makes RCL less expressive,
because RCL is not a general-purpose programming language;
it’s aimed at capturing simple data rather than arbitrarily complex functions.

[purescript]:  https://www.purescript.org/
[sempervivum]: https://github.com/ruuda/sempervivum
[musium]:      https://github.com/ruuda/musium
[rowpoly]:     https://github.com/purescript/documentation/blob/aba17dc1c240d2001f4c747430c46e823fb9987c/language/Types.md#row-polymorphism

## Dhall & Nickel

Two other typed configuration languages are [Dhall][dhall] and [Nickel][nickel].
I never used either language extensively,
so my opinion of their type systems is based mostly on the documentation
and superficial experiments,
not on experience.

Dhall is very Haskell-like.
[Its type system][dhall-types] is more rigid than RCL’s:
there is no subtyping (but there is polymorphism),
and the type system is completely static.
As such its type system has little in common with RCL.

[Nickel is gradually typed][nickel-types] like RCL,
but takes a very different approach to achieving that.
In RCL, all code is typechecked.
Every expression and subexpression has an inferred type (which can be `Any`),
and the [generalized subtype check][gsubck] provides a unified way
to defer any check that can’t be verified statically to runtime.
In Nickel,
the typechecker is only enabled on annotated expressions,
and there is [a more complex interaction][nickel-mix]
between statically typed and dynamically typed code
that also involves Nickel’s contract system.

I am of course biased and my understanding of Nickel is only superficial,
but to me Nickel’s approach to typing feels a bit ad-hoc.
It feels like several pieces put together,
rather than a system that emerges naturally
from a simple underlying principle.
In Cue and in RCL,
there is one principle that determines the entire type system,
and that makes them easier to explain,
and less likely to contain unexpected interactions.

[dhall]:        https://dhall-lang.org/
[dhall-types]:  https://docs.dhall-lang.org/tutorials/Language-Tour.html#types
[nickel]:       https://nickel-lang.org/
[nickel-types]: https://nickel-lang.org/user-manual/typing
[nickel-mix]:   https://nickel-lang.org/user-manual/typing#interaction-between-statically-typed-and-dynamically-typed-code
[gsubck]:       /2024/a-type-system-for-rcl-part-2-the-type-system#the-generalized-subtype-check

## C\#

While the current design of RCL’s type system has little in common with C#,
an earlier iteration featured a `Dynamic` type
that was named after and inspired by [`dynamic` in C#][csharp-dynamic].
This was before I discovered the [generalized subtype check][gsubck],
which provides an elegant way to determine what needs to be checked at runtime.
I kept the name `Dynamic` for a while,
but found it too verbose in practice,
and after I realized
that the lattice ordering should just be the subset relation,
`Any` was a more sensible name anyway.

[csharp-dynamic]: https://learn.microsoft.com/en-us/dotnet/csharp/advanced-topics/interop/using-type-dynamic

## H<!---->C<!---->L

As an example of how to _not_ do things,
[HCL’s `flatten` function][hcl-flatten] was one of the
droplets that triggered me to build RCL.
It’s documented as follows:

> Flatten takes a list and replaces any elements that are lists
> with a flattened sequence of the list contents.
> If any of the nested lists also contain directly-nested lists,
> these too are flattened recursively.
> Indirectly-nested lists, such as those in maps, are not flattened.

It is impossible to give a type signature for this function
in any sane type system,
because it violates a core principle of how flatten should work.
This is the correct type for flatten:

<pre><code class="sourceCode">flatten: <span class="dt">List</span>[<span class="dt">List</span>[<span class="dt">T</span>]] -> <span class="dt">List</span>[<span class="dt">T</span>]</code></pre>

This should work for _any_ type `T`,
and therefore `flatten` can’t flatten recursively,
because that would make its behavior dependent
on whether or not `T` is a list type.
This requirement is obvious in a typed language,
but it’s one of those things where if you implement something without a strong underlying principle,
a behavior may seem useful
(it can save a few calls on nested data, or maybe it was easy to implement this way),
but it backfires later (now it’s impossible to write generic functions,
and if you have a deeply nested list that you _want_ to only flatten one level,
you can’t do that).
R<!---->C<!---->L does not make this mistake:
[`flat_map` in RCL][rcl-flatmap] does not flatten recursively.

[hcl-flatten]: https://developer.hashicorp.com/terraform/language/functions/flatten
[rcl-flatmap]: https://docs.ruuda.nl/rcl/type_list/#flat_map

## Conclusion

In [part one][part1] of this series we outlined
what a type system for RCL should satisfy,
and in [part two][part2] we saw the type system itself.
In this post we discussed type systems that inspired RCL,
and how RCL’s type system differs from that of other configuration languages.
In the [final part][part4],
we will dive into the implementation of the typechecker.
If this post got you interested in RCL,
check out [the type system documentation][rcl-type-docs],
and [try RCL in your browser][rcl-playground]!

[rcl-type-docs]:  https://docs.ruuda.nl/rcl/types/
[rcl-playground]: https://rcl-lang.org/#try-it-yourself
