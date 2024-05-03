---
title: A type system for RCL, part 1: Introduction
header: A type system for RCL
subheader: Introduction
part: 1
lang: en-US
date: 2024-04-29
synopsis: TODO
---

<span class="run-in">I am [building][rcl-intro]</span> a new configuration language:
[RCL][rcl-lang].
From the start I intended it to have types,
but initially it was implemented as a completely dynamic language.
Now that I completed the initial version of a typechecker,
I thought it would be interesting to look at the type system and its implementation.
The type system is by no means complete,
in particular record types
and importing types across files are not yet supported,
but I think there is enough here for a post or two.
In part one we’ll explore the type system itself
and in part two we’ll dive into the internals of the typechecker.

[rcl-intro]: /2024/a-reasonable-configuration-language
[rcl-lang]:  https://rcl-lang.org/

My goal with this series is twofold:

 * **Sharing ideas and experiences.**
   None of the ideas in the type system are especially innovative or special,
   but the way in which they work together can still be interesting.
 * **Gathering feedback.**
   I feel pretty confident that the way the RCL interpreter is implemented makes sense,
   but I’m less sure about the type system.
   I studied program analysis and type and effect systems in university
   so none of this is completely new to me,
   but I have a feeling that for some of the choices I made,
   an expert more versed in the literature will say
   “Ah this is a well-known technique with this name”
   or “That approach doesn’t work, it has this problem.”

## What is RCL?

Before I can explain the type system,
let’s recap the language that we are typing.
R<!-- -->C<!-- -->L is a superset of json
that extends it into a simple functional language similar to [Nix][nix-lang].
Its goal is to enable abstraction and reuse in repetitive configuration
such as CI workflows,
deployment configuration such as Ansible playbooks and Kubernetes manifests,
and infrastructure-as-code tools like OpenTofu.
An RCL document consist of a single expression
and can be exported as json, yaml, or toml.

[nix-lang]: https://nixos.org/manual/nix/stable/language/index.html

The data model is that of json plus sets and functions.
Also, dictionary keys are not limited to strings,
they can be any value.
Aside from literals for these new values,
RCL adds let-bindings, list comprehensions, and a few other features to json.
Here is an example document:

```rcl
// TODO: Highlight.
let colors = ["blue", "green"];
let port_number = seq_no => 8000 + (100 * seq_no);
{
  deployments = [
    for i, color in colors.enumerate():
    {
      name = color,
      port = port_number(i),
    },
  ],
}
```

It evaluates to the following json document:

```json
{
  "deployments": [
    {"name": "red", "port": 8000},
    {"name": "blue", "port": 8100}
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
that can do dynamic dispatch on the values it encounters.
It does not have a compiler that _needs_ type information
to know what instruction to select,
or to emit the correct memory offset for a struct field.
So why do we need types at all?

Well, we don’t _need_ types.
But I want to have them for two reasons:

 * **To prevent bugs.**
   A type system enables moving invariants out of documentation
   and into the program where they can be mechanically enforced.
 * **To make code more self-documenting.**
   If you have ever worked in a large untyped Python or Javascript codebase,
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

It is no surprise that Mypy has taken the Python world by storm,
or that Typescript has largely displaced Javascript.
To keep a large codebase maintainable,
you need types.
But the snippet in the previous section is pretty clear on its own,
it would be a shame to make it more verbose than necessary,
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

```rcl
// What do we need to put on the ? to make the types explicit?
let xs: List[?] = [42, true, "yes"];
let y: ? = if xs.contains(21): "has-21" else null;
```

With type annotations removed, the above code is well-typed.
But that annotations are optional,
doesn’t mean that variables don’t have types.
So what _is_ the type of `xs` and `y`?
The way RCL deals with this is through a type lattice,
and the inferred type for both question marks is `Any`
— but I’m running ahead,
we’ll see more about the type lattice in the next post.

## Static typing

R<!---->C<!---->L is statically typed,
in the sense that it can report type errors in unreachable code.
For example,
the following program fails with a type error:

```
let string = "strings cannot be negated";
if false:
  // Error: Type mismatch. Expected Bool but found String.
  not string
else
  true
```

However,
although RCL enforces all type annotations,
it defers some type checks to runtime.
The following is fine:

```
let string: Any = "strings cannot be negated";
if false:
  not string
else
  true
```

In general,
the typechecker can encounter three cases:

* It can prove that the program contains a type error.
  In this case it reports the error.
* It can prove that the program is well-typed.
  In this case we proceed to evaluation.
* It can’t rule out a type error, but it can’t prove it either.
  In this case it inserts a runtime type check.

TODO: Move to next post.

## Blurring the line between static and runtime

For long-running daemons or programs deployed to users,
types are essential for building robust software.
Such programs need to be prepared to handle any input at runtime,
because if there is an unhandled runtime error,
there is no developer watching to fix the program.
A type system can help to force the programmer
to discover and handle edge cases ahead of time.

Configuration languages are on the other end of this spectrum.
An RCL program does not need to handle any inputs.
It has no inputs: all parameters are “hard-coded” into the program.
The program itself is the configuration file after all.

For an RCL program, there is no “run time”.
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

## Putting everything together

## Conclusion

Write
