---
title: A type system for RCL, part 2: The type system
header: A type system for RCL
subheader: The type system
part: 2
lang: en-US
date: 2024-04-29
synopsis: TODO
---

TODO: Write intro, headings, etc.

In this series:

 * [Part <abbr>I</abbr>: Introduction][part1]
 * [Part <abbr>II</abbr>: The type system][part2] (this post)
 * [Part <abbr>III</abbr>: The typechecker][part3]

TODO: Write that we track sourec spans for types and it's helpful.

TODO: Write no HM, no constraints.
[Swift is slow][swift-slow]

[swift-slow]: https://danielchasehooper.com/posts/why-swift-is-slow/

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

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-the-typechecker
