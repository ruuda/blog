---
title: A type system for RCL, part 1: Types
header: A type system for RCL
subheader: Types
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
 * **I’m looking for feedback.**
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
deployment configuration such as Ansible playbooks or Kubernetes manifests,
and infrastructure-as-code tools like OpenTofu.
An RCL document consist of a single expression
and can be exported as json, yaml, or toml.

[nix-lang]: https://nixos.org/manual/nix/stable/language/index.html

The data model is that of json plus sets and functions.
Furthermore, dictionary keys are not restricted to strings,
they can be any value.
Aside from literals for these values,
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
