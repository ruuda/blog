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
In a sense, it enables templating for structured data.
A common operation here is to build lists and dicts
out of other lists and dicts.
While RCL had several ways to do this,
I wasn’t satisfied with them.
I needed _unpack_.
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

In this post we’ll explore the trade-offs involved adding this feature.

## Why unpack?

 * Comprehensions are too verbose.
 * Binary operators are awkward to format.
 * The pipe operator is a mess in the typechecker.

## Design space

List the options here.

## Do we really need sets?

Maybe ... not?

## Conclusion

Need to write a conclusion here.

[rcl-lang]: https://rcl-lang.org
[v0110]:    https://docs.ruuda.nl/rcl/changelog/#0110
