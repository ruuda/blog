---
title: Adding unpack syntax to RCL
break: unpack syntax
date: 2025-11-13
lang: en-US
minutes: ??
synopsis: ??
run-in: R<!---->C<!---->L is a new configuration language
---

R<!---->C<!---->L is a new configuration language and json query tool.
It extends json into a simple functional language
that enables abstraction and reuse.
In a sense, it enables templating structured data.
A common operation here is to build lists and dicts
out of other lists and dicts.
While RCL had several ways to do this,
I wasn’t satisfied with them.
I wanted _unpack_.
After more than two years,
[v0.11.0][v0110] finally adds this feature,
and you can now use `..` and `...` to unpack lists and dicts:

```rcl
let xs = [3, 4];
let ys = [1, 2, ..xs, 5, 6];

// TODO: Less contrived example.
let defaults = { kind = "fruit", tasty = true };
let fruits = [
  { ...defaults, name = "banana" },
  { ...defaults, name = "grapefruit", tasty = false },
];
```

In this post we’ll explore the trade-offs involved in adding this feature.

## Why unpack?

Unpack does not add new technical capabilities to RCL.
What unpack can do, was already possible with comprehensions.
This list unpack and comprehension are equivalent:

```rcl
[1, 2, ..xs]
[1, 2, for x in xs: x]
```

And this dict unpack and comprehension are equivalent:

```rcl
{ id = 42, ...opts }
{ id = 42, for k, v in opts: k: v }
```

Furthermore, the union operator `|`
could be used for dict and set unions.
With that, the above dict could be written as:

```rcl
{ id = 42 } | opts
```

There are two problems with those options.
Comprehensions are too verbose,
and binary operators are awkward to format.

<!--
 * Comprehensions are too verbose.
 * Binary operators are awkward to format.
-->

The comprehensions aren’t even _that_ verbose,
but it’s enough friction that I dreaded writing them out every time,
and they obscure a simple operation
(splice a list, set, or dict into another one)
behind syntactic noise
(keywords, punctuation, and additional variables).
Even the `...` is a bit verbose for my taste,
but we’ll get to why it exists below.

The union operator doesn’t suffer from verbosity,
but there is no great way to format it when one of the sides is a multi-line dict,
and I don’t like how in a larger multi-line union
the first term looks different from the others.
Once we express unions with unpack,
everything becomes completely uniform.
Compare:

```rcl
// With union operator:
let widget =
  widget_default_opts
  | turbo_encabulator_opts
  | {
    id = 42,
    bearings = "spurving",
  };

// With unpack:
let widget = {
  ...widget_default_opts,
  ...turbo_encabulator_opts,
  id = 42,
  bearings = "spurving",
};
```

Sure,
the difference is superficial,
but it is one of those difference between a tool
that technically does what you need,
and one that’s a joy to use.
Moreover,
I expect the unpack version to be more self-explanatory to newcomers as well.
Aside from the formatting challenge,
the union operator had a fairly complex implementation in the type system.

Unpack solves all these problems neatly,
so for a long time it was clear to me that RCL needed unpack.
Why did it take so long to add?

## Wishlist

 * Sensible type system.
 * Syntactic distinction between dict and set.
 * `..` only.

## Do we really need sets?

Maybe ... not?

## Conclusion

Need to write a conclusion here.

[rcl-lang]: https://rcl-lang.org
[v0110]:    https://docs.ruuda.nl/rcl/changelog/#0110
