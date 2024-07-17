---
title: A type system for RCL, part 1: Introduction
header: A type system for RCL
subheader: Introduction
part: 1
lang: en-US
date: 2024-07-17
synopsis: I am adding a type system to RCL, my configuration language. In this post I explain what I want from the type system.
---

<span class="run-in">I am [building][rcl-intro]</span> a new configuration language:
[RCL][rcl-lang].
From the start I intended it to have types,
but initially it was implemented as a completely dynamic language.
Now that I’m adding a typechecker,
I thought it would be interesting to look at the type system and its implementation.
The type system is by no means complete
— in particular record types
and importing types across files are not yet supported —
but there is enough to fill a few posts.
This introduction explores RCL
and what problems a type system for RCL should and should not solve.
In part two and three we’ll explore the type system itself and related work,
and in part four we’ll dive into the internals of the typechecker.

[rcl-intro]: /2024/a-reasonable-configuration-language
[rcl-lang]:  https://rcl-lang.org/

My goal with this series is twofold:

 * **Sharing ideas and experiences.**
   None of the ideas in the type system are especially innovative or special,
   but the way in which they work together can still be interesting.
 * **Gathering feedback.**
   I feel pretty confident that the way the RCL interpreter is implemented makes sense,
   but I’m less sure about the type system.
   I studied program analysis and type and effect systems during my master’s
   so none of this is completely new to me,
   but I have a feeling that for some of the choices I made,
   an expert more versed in the literature will say
   “Ah this is a well-known technique with this name”
   or “That approach doesn’t work, it has this problem.”

In this series:

 * [Part <abbr>I</abbr>: Introduction][part1] (this post)
 * Part <abbr>II</abbr>: The type system — _to be published in the next few days_
 * Part <abbr>III</abbr>: Related work — _to be published in the next few days_
 * Part <abbr>IV</abbr>: The typechecker — _to be published in the next few days_

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/a-type-system-for-rcl-part-4-the-typechecker

## What is RCL?

Before I can explain the type system,
let’s recap the language that we are typing.
R<!-- -->C<!-- -->L is a superset of json
that extends it into a simple functional language similar to [Nix][nix-lang].
Its goal is to enable abstraction and reuse in repetitive configuration
such as CI workflows,
deployment configuration such as Ansible playbooks and Kubernetes manifests,
and infrastructure-as-code tools like OpenTofu.
An RCL document consists of a single expression
and can be exported to json, yaml, or toml.
Any time somebody contemplates templating yaml,
they should consider using RCL instead.

[nix-lang]: https://nixos.org/manual/nix/stable/language/index.html

The data model is that of json plus sets and functions.
Also, dictionary keys are not limited to strings,
they can be any value.
Aside from literals for these new values,
RCL adds let-bindings, list comprehensions, and a few other features to json.
Here is an example document:

<pre><code class="sourceCode"><span class="kw">let</span> colors = [<span class="st">"blue"</span>, <span class="st">"green"</span>];
<span class="kw">let</span> port_number = offset => <span class="dv">8000</span> + (<span class="dv">100</span> * offset);
{
  <span class="n">deployments</span> = [
    <span class="kw">for</span> i, color <span class="kw">in</span> colors.<span class="fu">enumerate</span>():
    {
      <span class="n">name</span> = color,
      <span class="n">port</span> = port_number(i),
    }
  ],
}
</code></pre>

It evaluates to the following json document:

```json
{
  "deployments": [
    {"name": "blue", "port": 8000},
    {"name": "green", "port": 8100}
  ]
}
```

For a more gradual introduction,
check out [the tutorial][tutorial],
or try [the interactive demos][demo] on the the website.

[tutorial]: https://docs.ruuda.nl/rcl/tutorial/
[demo]:     https://rcl-lang.org/#try-it-yourself

## Why types?

R<!---->C<!---->L is implemented as a tree-walking interpreter
that can dispatch on the values it encounters.
It does not have a compiler that _needs_ type information
to know what instruction to select,
or to emit the correct memory offset for a struct field.
So why do we need types at all?

Well, we don’t _need_ types.
But I want to have them for two reasons:

 * **To prevent bugs.**
   A type system enables moving invariants out of documentation
   and into the program where they can be mechanically enforced.
   In configuration in particular,
   a type system can ensure that all construction sites are updated when renaming a field,
   or that a string belongs to a set of allowed values.
 * **To make code more self-documenting.**
   If you have ever worked in a large untyped Python or JavaScript codebase,
   you might recognize this problem:
   you’re modifying a function that takes an argument named `user`,
   and you need the user’s display name.
   You can see that the function is accessing the `id` field,
   but where is the name?
   Is the field named `username`, `handle`, `displayName`?
   You look at the call site, but alas,
   seven callers further it turns out it’s some dynamically constructed dict,
   and part of it comes from an embedded `select * from users` SQL query.
   Printf debugging it is ...
   It would be nice if there was a type annotation on the function argument
   where your editor’s jump-to-definition can just show you the type and its fields.

It is no surprise that TypeScript has largely displaced JavaScript,
and that Mypy has taken the Python world by storm.
To keep a large codebase maintainable,
you need types.

The snippet in the previous section is pretty clear on its own though.
It would be a shame to make it more verbose than necessary,
especially for a configuration language that tries to eliminate boilerplate.
So type annotations in RCL are optional.
The type system is gradual,
so you can clarify and enforce types when necessary,
but you don’t have to specify them in straightforward code.

## Typing json

The purpose of RCL is to output configuration files for other tools.
These tools can demand any schema.
If RCL placed limitations on that,
it would not be a very useful configuration language.
This means that the type system must be able to deal with
constructs that some type systems would reject,
such as heterogeneous lists,
or if-else expressions where the then-branch
returns a different type than the else-branch.

<pre><code class="sourceCode"><span class="co">// What do we need to put on the ? to make the types explicit?</span>
<span class="kw">let</span> xs: <span class="dt">List</span>[<span class="dt">?</span>] = [<span class="dv">42</span>, <span class="kw">true</span>, <span class="st">"yes"</span>];
<span class="kw">let</span> y: <span class="dt">?</span> = <span class="kw">if</span> xs.contains(<span class="dv">21</span>): <span class="st">"has-21"</span> <span class="kw">else null</span>;
</code></pre>

With type annotations removed, the above code is well-typed.
But that annotations are optional,
doesn’t mean that variables don’t have types.
So what _is_ the type of `xs` and `y`?
The way RCL deals with this is through a type lattice,
and the inferred type for both question marks is `Any`
— but I’m running ahead,
we’ll see more about the type lattice in the next post.

## Static vs. runtime

For long-running daemons or programs deployed to users,
types are essential for building robust software.
Such programs need to be prepared to handle _any_ situation at runtime,
because if there is an unhandled runtime error,
there is no developer watching to fix the program.
A type system can help the programmer
to discover and handle edge cases ahead of time.

Configuration languages are on the other end of this spectrum.
An RCL program does not need to be able to handle any situation,
it needs to handle exactly one.
The program doesn’t even have any inputs:
all parameters are “hard-coded” into it.
The program itself is the configuration file after all.

For an RCL program, there is no “run-time”.
A user will run RCL to generate some configuration,
and if that succeeds,
RCL is out of the picture.
Internally it has separate typechecking and evaluation phases,
but these run directly after one another,
and the user will see static errors and runtime errors at the same moment.

Because of this specialized use case,
runtime errors in RCL are not nearly as bad as in some other languages.
If we can’t prevent an error statically,
we can defer to a runtime check,
and the user will still learn about the error at compile time.
Runtime errors are static errors in RCL.

<!-- (A bit like how [Haskell is a dynamically typed, interpreted language][typing-interview].) -->

[typing-interview]: https://aphyr.com/posts/342-typing-the-technical-interview

## What should be well-typed?

The type system is a new addition to RCL.
Although it is a goal for RCL to be able to represent any json document,
it is not the goal that any expression
that could be evaluated prior to the addition of the typechecker,
is well-typed.
For example,
the following program has a type error,
even though it can be evaluated with the typechecker disabled:

<pre><code class="sourceCode"><span class="kw">let</span> ints = [
  <span class="kw">for</span> x <span class="kw">in</span> [<span class="dv">1</span>, <span class="dv">2</span>, <span class="dv">3</span>]:
  <span class="kw">if</span> x > <span class="dv">10</span>:
  x
];
[<span class="kw">for</span> i <span class="kw">in</span> ints: <span class="kw">not</span> i]
</code></pre>

R<!---->C<!---->L reports the following error:

<pre><code class="sourceCode">  <span class="dt">|</span>
6 <span class="dt">|</span> [for i in ints: not i]
  <span class="dt">|</span>                     <span class="dt">^</span>
<span class="dt">Error:</span> Type mismatch. Expected <span class="dt">Bool</span> but found <span class="dt">Int</span>.

  <span class="st">|</span>
6 <span class="st">|</span> [for i in ints: not i]
  <span class="st">|</span>                 <span class="st">^~~</span>
<span class="st">Note:</span> Expected Bool because of this operator.

  <span class="st">|</span>
2 <span class="st">|</span>   for x in [1, 2, 3]:
  <span class="st">|</span>             <span class="st">^</span>
<span class="st">Note:</span> Found Int because of this value.
</code></pre>

It is true that `not` cannot be applied to integers,
but that type error is not exposed at runtime because `ints` is empty,
so without the typechecker,
evaluation succeeds.
I am fine with rejecting pathological code like this.
After all,
trying to negate an integer is probably a bug,
even if the code path is unreachable.

## Putting it together

R<!---->C<!---->L is a new configuration language
that aims to reduce configuration boilerplate
by extending json into a simple functional language
that enables abstraction and reuse.
I am adding support for type annotations and a typechecker to it.
What do I want from the type system?

 * **It should help to prevent bugs**,
   by moving invariants out of comments and into the program,
   where they can be enforced.
 * **It should make code more self-documenting**,
   without forcing type annotations onto code that is already clear by itself.
 * **Any json value should be well-typed**,
   including things like heterogeneous lists,
   because RCL must be able to generate any json value as output.
 * **It is okay to defer to runtime checks**.
   As a configuration language,
   typechecking and evaluation happen in the same session,
   so to a user there is little difference
   between a static type error and a runtime type error.

In the remainder of this series,
we’ll see how RCL achieves this.
<!--
In [part two][part2] we will look at the type system,
and in [part three][part3] at some related type systems that inspired RCL.
Finally, in [part four][part4]
we will look at the implementation of the typechecker itself.
-->
In part two we will look at the type system,
and in part three at some related type systems that inspired RCL.
Finally, in part four
we will look at the implementation of the typechecker itself.

_These next parts will be published in the coming days.
In the meantime,
if this post got you interested in RCL,
check out [the type system documentation][rcl-type-docs],
and [try RCL in your browser][rcl-playground]!_

[rcl-type-docs]:  https://docs.ruuda.nl/rcl/types/
[rcl-playground]: https://rcl-lang.org/#try-it-yourself
[rcl-jq]:         /2024/a-reasonable-configuration-language/#an-unexpected-jq-replacement
[rcl-v04]:        https://docs.ruuda.nl/rcl/changelog/#040
