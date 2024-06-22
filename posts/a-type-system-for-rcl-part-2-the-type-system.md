---
title: A type system for RCL, part 2: The type system
header: A type system for RCL
subheader: The type <span class="dlig">system</span>
part: 2
lang: en-US
date: 2024-04-29
synopsis: TODO
---

TODO: Write intro, headings, etc.

In this series:

 * [Part <abbr>I</abbr>: Introduction](/2024/a-type-system-for-rcl-part-1-introduction)
 * [Part <abbr>II</abbr>: The type system](/2024/a-type-system-for-rcl-part-2-the-type-system) (this post)
 * [Part <abbr>III</abbr>: The typechecker](/2024/a-type-system-for-rcl-part-3-the-typechecker)

TODO: Write that we track sourec spans for types and it's helpful.

TODO: Write no HM, no constraints.
[Swift is slow][swift-slow]

[swift-slow]: https://danielchasehooper.com/posts/why-swift-is-slow/

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
