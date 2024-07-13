---
title: A type system for RCL, part 3: Related work
header: A type system for RCL
subheader: Related work
part: 3
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
 * [Part <abbr>II</abbr>: The type system][part2]
 * [Part <abbr>III</abbr>: Related work][part3] (this post)
 * [Part <abbr>IV</abbr>: The typechecker][part4]

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/a-type-system-for-rcl-part-4-the-typechecker

In part one we looked at what I want from a type system for RCL,
and in part two we saw how RCL tries to achieve those goals.
The resulting type system is not a completely new invention,
it’s based explicitly and implicitly
on other languages that I’ve worked with,
and ideas that I have been exposed to.
R<!---->C<!---->L is not the first typed json superset,
nor the first typed configuration language.
In this post I want to take a break from RCL itself
and acknowledge some of the work that inspired RCL,
and contrast RCL with alternatives in this space.

## Python and Mypy

[**Python and Mypy**][mypy]
Mypy is one of the main indirect influences for RCL’s type system.
It is the most mature typechecker for Python,
and it is the gradual type system that I have most experience with.
(I’ve been using it since Python 2,
back when annotations were still in comments.)
The type system differs from RCL in important ways
(in particular, RCL enforces annotations at runtime, unlike Python),
but the look and feel of RCL is intentionally very Python-like,
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
That part is not relevant
for a functional language with immutable bindings,
but the lattice part is useful.
I can’t find an obvious authoritative source to link to,
[but I found this slide deck][jhage-uu] that treats the topic.

[jhage-uu]: https://www.macs.hw.ac.uk/~jh2054/downloads/pythonbalancing-talk.pdf

## Cue
[**Cue**][cue]
Cue is another typed configuration language,
and its types [form a lattice as well][cue-lattice].
In fact, the lattice is very similar to the one in RCL.
Cue goes a step further by merging the type and value universes,
and I really like how that approach can be used
both for constraining values and reducing boilerplate.
Though the approach is really elegant and powerful,
it requires a bit of a mental leap.
For RCL,
I wanted something that is completely obvious and boring
to somebody familiar with more traditional imperative languages;
something that you don’t need to learn because you already know it,
even when you see it for the first time.
As a language,
RCL is more similar to [Jsonnet][jsonnet]
(very similar actually),
though Jsonnet does not have static types.

[cue]:         https://cuelang.org/
[cue-lattice]: https://cuelang.org/docs/concept/the-logic-of-cue/
[jsonnet]:     https://jsonnet.org/

[**TypeScript**][typescript]<br>
In a sense,
TypeScript is to JavaScript what Mypy is to Python.
It _should_ be very relevant for me,
because TypeScript is a superset of json,
so like RCL its type system has to be able to describe arbitrary json data.
I haven’t studied TypeScript’s type system much though.
I try hard to avoid the JavaScript and NPM ecosystem,
so I haven’t used TypeScript much except in a few small applications,
therefore I can’t really comment on its type system.

[typescript]: https://www.typescriptlang.org/

[**PureScript**][purescript]<br>
PureScript is another typed functional language.
It compiles to and interoperates with JavaScript,
so it has good support for modelling JavaScript objects
(dicts/records in RCL).
I’ve written two applications in it
([a plant watering tracker][sempervivum] and [a music player][musium]),
and I feel more comfortable with it than TypeScript.
I haven’t encountered the need for [row polymorphism][rowpoly] though.
I need to dive into this deeper when I get to adding record types to RCL,
but my feeling is that row types may be a bit too advanced.
I want RCL to be obvious to understand
even for people who don’t have a deep background in typed functional programming.

[purescript]:  https://www.purescript.org/
[sempervivum]: https://github.com/ruuda/sempervivum
[musium]:      https://github.com/ruuda/musium
[rowpoly]:     https://github.com/purescript/documentation/blob/aba17dc1c240d2001f4c747430c46e823fb9987c/language/Types.md#row-polymorphism

**[Dhall][dhall] and [Nickel][nickel]**<br>
Two other typed configuration languages are Dhall and Nickel.
I never used either language extensively
so my opinion of their type systems is based mostly on the documentation,
and not on experience.
Dhall is very Haskell-like.
[Its type system][dhall-types] is more rigid than RCL:
there is no subtyping,
and the type system is completely static.
As such it does not have much in common with RCL
except for the parts that are inevitable to have a useful type system.
[Nickel is gradually typed][nickel-types] like RCL,
but takes a very different approach to the implementation.
In RCL all code is typechecked.
Every expression and subexpression has an inferred type (which can be `Any`),
and the generalized subtype check provides a unified way
to defer any check that can’t be verified statically to runtime.
In Nickel,
the typechecker is only enabled on annotated expressions,
and there is [a more complex interaction][nickel-mix]
between statically typed and dynamically typed code
that also involves Nickel’s contract system.
I am of course biased and my understanding of Nickel is only superficial,
but to me it feels a bit ad-hoc
— I don’t see a simple underlying principle —
while for Cue and RCL
there is one simple principle that underlies the entire type system,
which makes it easier to explain,
and less likely to contain unexpected interactions.

[dhall]:        https://dhall-lang.org/
[dhall-types]:  https://docs.dhall-lang.org/tutorials/Language-Tour.html#types
[nickel]:       https://nickel-lang.org/
[nickel-types]: https://nickel-lang.org/user-manual/typing
[nickel-mix]:   https://nickel-lang.org/user-manual/typing#interaction-between-statically-typed-and-dynamically-typed-code
