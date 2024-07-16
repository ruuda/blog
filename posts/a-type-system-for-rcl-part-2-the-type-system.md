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
 * [Part <abbr>III</abbr>: Related work][part3]
 * [Part <abbr>IV</abbr>: The typechecker][part4]

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/a-type-system-for-rcl-part-4-the-typechecker

In part one we looked at what I want from a type system for RCL.
In this part we’ll look at the type system so far.
The type system is a work in progress.
I plan to still add record types and type aliases,
and one thing I haven’t yet figured out
is how importing types from files should work.
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

## Foundation

Two major choices affect RCL’s type system:

 1. **Types constrain values, but values don’t have unique types.**
    The type system specifies whether a value _fits_ a type,
    but the same value can fit multiple types.
    Variables that have different types can refer to values that are equal.
 2. **Type inference is forward-only, and mostly bottom-up.**
    The typechecker assigns a concrete type to every bound variable;
    there is no Hindley–Milner-style unification,
    or constraint propagation that makes type information flow backwards.
    Although the typechecker always infers _some_ type,
    the inferred type can be less precise
    than what backwards inference could find.
    Forward-only inference keeps the typechecker
    [fast and tractable for humans][swift-slow].
    After all, RCL tries to be a _reasonable_ configuration language:
    humans should be able to reason about whether a program is well-typed.

[swift-slow]:  https://danielchasehooper.com/posts/why-swift-is-slow/

## Types constrain values

What does it mean that the same value can fit multiple types?
It means that all of these are well-typed:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">Int</span> = <span class="dv">0</span>;
<span class="kw">let</span> b: <span class="dt">Any</span> = <span class="dv">0</span>;
<span class="kw">let</span> c: <span class="dt">Union</span>[<span class="dt">Int</span>, <span class="dt">String</span>] = <span class="dv">0</span>;
</code></pre>

Collections are another example.
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
so we have a partial order on types.
Which brings us to the next topic:
types form a _lattice_.

## The type lattice

A lattice — strictly speaking a _join-semilattice_ in this case —
is a partially ordered set with an operation called _join_
that returns the least upper bound of two elements.
Part of the type lattice looks like this:

![A part of the type lattice.](/images/lattice.svg)

The bottom of the lattice is `Void`, the uninhabited type.
It corresponds to the empty set.
The top of the lattice is `Any`, the type that any value fits.
It corresponds to the set of all values.
In between we have primitive types like `Int` and `String`,
but also collection types like `List[Int]`,
a list of integers.

The partial order on types is the subset relationship on the underlying sets.
If every value of type `T` fits type `U`,
then we say that `T` is a subtype of `U`, written `T` ≤ `U`.
Note that `Null` is not a subtype of the primitive types.
Unlike many other languages,
RCL does not have implicit nullability.

The join operation is useful for bottom-up type inference.
For example,
when the typechecker encounters a list `[t,` `u]`,
where `t:` `T` and `u:` `U`,
it infers that the list has type `List[V]`,
where `V` is the join of `T` and `U`.

## Static and gradual typing

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
In this sense, RCL is gradually typed.
The following is fine:

<pre><code class="sourceCode"><span class="kw">let</span> string: <span class="dt">Any</span> = <span class="st">"strings cannot be negated"</span>;
<span class="kw">if</span> <span class="kw">false</span>: <span class="kw">not</span> string <span class="kw">else</span> <span class="kw">true</span>
</code></pre>

When checking an expression,
the typechecker can encounter three cases:

1. It can prove that the expression is well-typed.
   Evaluation is guaranteed to succeed.
2. It can prove that evaluation would fail with a type error.
   In this case it reports the error.
3. The check is inconclusive:
   it can’t rule out a type error, but it can’t prove it either.
   In this case the typechecker inserts a runtime type check.

Checking whether an expression fits a type is the role of the _subtype check_.

## The subtype check

If RCL were a completely statically typed language,
the typechecker’s job would be to perform a _subtype check_ on expressions.
For every expression it visits,
it has an inferred actual type for that expression,
and a specified expected type.
Consider for example:

<pre><code class="sourceCode"><span class="kw">let</span> a: <span class="dt">Int</span> = <span class="dv">0</span>;
<span class="kw">let</span> b: <span class="dt">Any</span> = a;
<span class="kw">let</span> c: <span class="dt">Int</span> = b;
</code></pre>

 * On the first line
   the inferred type of the right-hand side is `Int`,
   because integer literals are integers.
   The expected type is also `Int`,
   due to the type annotation.
   We have `Int` ≤ `Int`,
   so the typecheck passes.
 * On the second line
   the inferred type is `Int`, because `a:` `Int`.
   The expected type is `Any`.
   We have `Int` ≤ `Any`,
   so again the typecheck passes.
 * On the third line
   the inferred type is `Any`, because `b:` `Any`.
   The expected type is `Int`.
   But it does **not** hold that `Any` ≤ `Int`,
   so the typecheck fails;
   the program contains a type error.

This is a bit of a shame though.
We can _see_ that `c` is going to be bound to the integer `0`.
There is no runtime type error here.
Yet, the typechecker cannot see that,
so it has to report a static error.

Some languages offer a way out through runtime type inspection,
with e.g. an `isinstance` check.
If RCL had one, it might look like this:

<pre><code class="sourceCode"><span class="kw">let</span> c: <span class="dt">Int</span> = <span class="kw">if</span> <span class="fu">isinstance</span>(b, <span class="dt">Int</span>): b <span class="kw">else</span> <span class="dv">0</span>;
</code></pre>

A typechecker that is aware of `isinstance`
would change the type of `b` from `Any` to `Int` inside the then-branch,
and then the expression again typechecks.
R<!---->C<!---->L does not have a user-exposed `isinstance` check,
but it does perform runtime type checks under the hood.

[static]: /2024/a-type-system-for-rcl-part-1-introduction#static-vs.-runtime

## The generalized subtype check

R<!---->C<!---->L knows when to insert a runtime type check
because the typechecker performs what I call a _generalized subtype check_.
The checker needs to answer the question
“does this expression evaluate to a value of type `U`?”
It does that using a subtype check `T` ≤ `U`,
where `T` is the inferred type of the expression.
Traditionally a check can reach two possible conclusions:

 1. `T` ≤ `U`, I can prove that the program will not encounter runtime type errors.
 2. `T` ≤ `U` does not hold,
    I failed to prove the absence of runtime type errors.
    Maybe it fails, maybe not.

As we saw before in the lattice section,
the inferred type `T` is an upper bound.
It may be too pessimistic.
For a language where runtime type errors are fatal,
the typechecker has to be pessimistic,
so it reject programs in case **2**.
But in a configuration language,
[runtime errors are static errors][static].
The important thing
is that we don’t allow execution to continue
when the type annotation is violated.
Whether we prevent the violation statically or at runtime
makes little difference to the user,
because typechecking and runtime happen in the same session.
So maybe we could just try?
What if the typechecker could reach three different conclusions?

 1. **Well-typed** — For all `t:` `T`, we have `t:` `U`.
 2. **Static error** — For all `t:` `T`, `t` does not fit `U`.
 3. **Inconclusive** — There exist a `t0:` `T` that fits `U`,
    and `t1:` `T` that does not fit `U`.

![Venn diagrams that illustrate the three cases.](/images/subtypes.svg)

R<!---->C<!---->L implements this generalized subtype check,
and when the result is inconclusive,
it inserts a runtime check
that verifies that the value fits the expected type.

## Variance

R<!---->C<!---->L has types that have a subtype relation,
and it has generic types like `List[T]`, `Dict[K,` `V]`, and function types.
At the intersection of subtypes and generics,
we have _variance_.
`List[T]` is covariant in `T`:
we have `List[T]` ≤ `List[U]` if and only if `T` ≤ `U`.
This neat property falls out of the subset definition of the partial order on sets.
How does covariance behave under the generalized subtype check?
Let’s write `T` ≤ `U` for case **1** as defined before (well-typed),
`T` !≤ `U` for case **2** (static type error),
and `T` ~ `U` for case **3** (inconclusive).
It would be nice if it worked like this:

 1. If `T` ≤ `U`, then `List[T]` ≤ `List[U]`.
 2. If `T` !≤ `U`, then `List[T]` !≤ `List[U]`.
 3. If `T` ~ `U`, then `List[T]` ~ `List[U]`.

Unfortunately,
this is not the case
according to our earlier definition of the generalized subtype check.
We have `Int` !≤ `Bool`,
so we would like to say that `List[Int]` !≤ `List[Bool]`,
but the empty list is an instance of both,
so we have to conclude that `List[Int]` ~ `List[Bool]`.
That’s a shame,
because it means that we can report fewer type errors statically.
And to be fair, when you write

<pre><code class="sourceCode"><span class="kw">let</span> xs = [
  <span class="kw">for</span> x <span class="kw">in</span> [<span class="dv">1</span>, <span class="dv">2</span>, <span class="dv">3</span>]:
  <span class="kw">if</span> x > <span class="dv">10</span>:
  x
];
<span class="kw">let</span> ys: <span class="dt">List</span>[<span class="dt">Bool</span>] = xs;
</code></pre>

that _is_ probably a bug,
even though there is no runtime type error.
So I opted to make the generalized subtype check in RCL
respect this generalized notion of covariance for `List`,
and report a static type error in the case above.

This is one of the places in the type system
where I’m not sure if my approach is right.
There is some freedom in implementing the subtype check,
and when things get too complex,
it’s always possible to say either “inconclusive, we’ll insert a runtime check”
for code that is useful in practice,
or “static error even though it wouldn’t fail in strictly all cases”
for contrived examples like the one above.
So far this has been working fine,
but this feels like one of those things
where if you make ad-hoc decisions
without a strong underlying principle,
they backfire and cause interactions that don’t make sense.
For example,
JavaScript defined the `==` operator
in an ad-hoc way without transitivity in mind,
and even though the definition might have seemed useful at first,
it [creates logical inconsistencies][js-trinity]
that make JavaScript difficult to reason about.
I want RCL to be easy to reason about,
and using set operations to define the generalized subtype check
seemed like a strong guiding principle.
Unfortunately I think it makes the typechecker too weak,
and breaking covariance is maybe a more surprising behavior
than rejecting an assignment of `List[Int]` to `List[Bool]`.
I would love to hear from people who have encountered this problem before.

[js-trinity]: https://javascriptwtf.com/wtf/javascript-holy-trinity

## Bottom-up inference

I mentioned before that inference is _mostly_ bottom-up.
This means that in the absence of type annotations,
inference is syntax-directed.
An integer literal has type `Int`,
a string literal has type `String`, etc.
For a list literal,
the inferred type is the join of the inferred element types,
for an if-else expression
the inferred type is the join of the then and else-branches, etc.
Because values don’t have a unique type,
we have some freedom in what we infer.
What should the inferred type of this dict literal be?

<pre><code class="sourceCode"><span class="kw">let</span> server = {
  <span class="n">hostname</span> = <span class="st">"webserver"</span>,
  <span class="n">enable_sshd</span> = <span class="kw">true</span>,
  <span class="n">expose_ports</span> = [<span class="dv">22</span>, <span class="dv">80</span>, <span class="dv">443</span>],
};
</code></pre>

If we had record types,
we might infer this record type:

<pre><code class="sourceCode">{
  hostname: <span class="dt">String</span>,
  enable_sshd: <span class="dt">Bool</span>,
  expose_ports: <span class="dt">List</span>[<span class="dt">Int</span>],
}
</code></pre>

In fact, this is what TypeScript
(another language that adds types to a json superset)
does.
But it would be equally valid to infer `Dict[String,` `Any]`.
This is a less precise type,
but faster to infer.
Especially for a list of records,
incrementally constructing the inferred type
and then joining those for every element,
is a lot to churn through and allocate
— even if algorithmically it can be done in linear time.
For a general-purpose language like TypeScript that might be acceptable,
but in a configuration language like RCL,
an input could be a data file with thousands of elements
(say, a collection of user accounts or servers).
Another problem with these inferred record types
is that they can grow very big,
which can make error messages unwieldy
— a problem I have encountered first-hand in TypeScript and PureScript.
Finally,
[`rcl` `jq`][rcl-jq] may need to search through a large json file,
and it would be wasteful to infer a precise type
when that type never gets used.
Perhaps my concerns are premature,
but I decided that RCL will never infer record types.
So if more complex types can’t be inferred,
where do they come from?

[rcl-jq]: https://docs.ruuda.nl/rcl/rcl_query/

## Top-down validation

In RCL,
type checking and type inference are fused together.
Checking an expression takes two inputs:

 * The expression itself.
 * An expected type `U`.

The result of the fused check-infer can then be one of three cases:

 1. The expression is well-typed,
    and it has inferred type `T` where `T` ≤ `U`.
 2. The expression is ill-typed,
    and here is a type error that explains why `T` !≤ `U`.
 3. Inconclusive,
    we need to insert a runtime check,
    and we should validate against type `T` where `T` ≤ `U`.

In most cases the expected type `U` is `Any`,
but for example in the condition of an assertion or if-else expression,
the expected type is `Bool`.
The other case where the expected type can be different,
is in let bindings that contain a type annotation.
This is how record types can enter the type system.

This approach to inference does have limitations
compared to a constraint-based approach,
in particular for inference of function types.
When checking a function body,
the typechecker needs to have a type for the arguments,
and because we don’t have type variables,
if there is no annotation,
it has to assume `Any`.

<pre><code class="sourceCode"><span class="co">// Inferred as (Any) -> Int, with a runtime check before the</span>
<span class="co">// multiplication that ensures the argument is an integer.</span>
<span class="kw">let</span> f = x => x * <span class="dv">2</span>;

<span class="co">// With an explicit annotation, we can get a more precise type,</span>
<span class="co">// and eliminate the need for a runtime check in the body.</span>
<span class="kw">let</span> g: (<span class="dt">Int</span>) -> <span class="dt">Int</span> = x => x * <span class="dv">2</span>;
</code></pre>

So far this looks like an acceptable trade-off to make.

## Referential transparency

One unfortunate consequence of forward-only bottom-up inference,
is that it can in a sense break referential transparency.
Giving a subexpression a name by extracting it into a let-binding
can change the way errors are reported.
It does not change the behavior of a well-typed program
(which would be a cardinal sin for a functional language),
but it still makes me uneasy.

For example,
in the program below we can report a static type error,
and pinpoint the source of the problem:

<pre><code class="sourceCode"><span class="kw">let</span> xs: <span class="dt">List</span>[<span class="dt">Int</span>] = [<span class="dv">1</span>, <span class="dv">2</span>, <span class="st">"three"</span>];
</code></pre>

<pre><code class="sourceCode">  <span class="dt">|</span>
1 <span class="dt">|</span> let xs: List[Int] = [1, 2, "three"];
  <span class="dt">|</span>                            <span class="dt">^~~~~~~</span>
<span class="dt">Error:</span> Type mismatch. Expected <span class="dt">Int</span> but found <span class="dt">String</span>.

  <span class="st">|</span>
1 <span class="st">|</span> let xs: List[Int] = [1, 2, "three"];
  <span class="st">|</span>              <span class="st">^~~</span>
<span class="st">Note:</span> Expected Int because of this annotation.
</code></pre>

If we move the type annotation,
then now `xs` is inferred as `List[Any]`,
and the assignment to `ys` causes a runtime check to be inserted.
This runtime check
no longer has access to the source location that produced the value,
only to the value itself,
so although it still reports a type error,
the error looks different now:

<pre><code class="sourceCode"><span class="kw">let</span> xs = [<span class="dv">1</span>, <span class="dv">2</span>, <span class="st">"three"</span>];
<span class="kw">let</span> ys: <span class="dt">List</span>[<span class="dt">Int</span>] = xs;
</code></pre>

<pre><code class="sourceCode">  <span class="dt">|</span>
2 <span class="dt">|</span> let ys: List[Int] = xs;
  <span class="dt">|</span>                     <span class="dt">^~</span>
in value
at index <span class="dv">2</span>
<span class="dt">Error:</span> Type mismatch. Expected a value that fits this type:

  <span class="dt">Int</span>

But got this value:

  <span class="st">"three"</span>

  <span class="st">|</span>
2 <span class="st">|</span> let ys: List[Int] = xs;
  <span class="st">|</span>              <span class="st">^~~</span>
<span class="st">Note:</span> Expected Int because of this annotation.
</code></pre>

Having different ways to surface the same type error is unfortunate,
but I think I can live with it.
On the one hand it’s hard to justify
that giving a value a name can affect behavior,
but on the other hand,
“inference is forward-only” is a simple rule to explain,
and I think that making
“put annotations close to the definition” a best practice
can help to mitigate this limitation.
For now this looks like an acceptable trade-off,
but I’ll have to see how it pans out with record types
and larger codebases that import types and values from multiple files.
I would love to hear from people who encountered this trade-off before.

## Conclusion

R<!---->C<!---->L is a new configuration language that I am building.
As we saw in [part one][part1],
being a configuration language
puts constraints on how the type system should behave.
In this post we looked at the type system so far.

 * **The type system is gradual, and based on a lattice.**
   There is a subtype relationship between types,
   and the same value can be described by multiple types,
   e.g. `42:` `Int` or `42:` `Any`.
   The lattice enables RCL to type constructs
   that would be disallowed in more rigid static type systems,
   but which may occur in configuration.
   For example,
   RCL infers the type `List[Any]` for the heterogeneous list `[1,` `true]`.
 * **Type inference is forward-only and mostly bottom-up.**
   The absence of unification has limitations,
   but it also keeps inference fast and type errors clear.
   The lattice’s _join_ operation is helpful for type inference
   of collections and if-else expressions.
 * **Typechecking is based on a generalized subtype check.**
   We check expressions against an expected type.
   If the inferred type is a subtype of the expected type,
   the expression is well-typed.
   If the types are disjoint,
   that’s a static error.
   When the types overlap but are not subtypes,
   the typechecker inserts a runtime check.
   This core principle
   enables the type system to handle all gradual typing in a uniform way.

The type system is based directly and indirectly
on ideas from other languages and systems.
In [the next part][part3] we’ll look at prior work,
and we will contrast RCL’s type system
with that of other configuration languages.
Finally,
in [part four][part4] we will look at the typechecker itself.
