---
title: A type system for RCL, part 4: The typechecker
header: A type system for RCL
subheader: The typeche<span class="dlig">ck</span>er
part: 4
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
 * [Part <abbr>III</abbr>: Related work][part3]
 * [Part <abbr>IV</abbr>: The typechecker][part4] (this post)

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/a-type-system-for-rcl-part-4-the-typechecker
[gsubck]: /2024/a-type-system-for-rcl-part-2-the-type-system#the-generalized-subtype-check

In part one we looked at what I want from a type system for RCL,
and in part two we saw what the resulting type system looks like.
In this part we’ll take a closer look at how the typechecker is implemented.

## Rust

R<!---->C<!---->L is written in Rust.
TODO: Does it matter?

## Background

Good ideas appear obvious in hindsight,
but that doesn’t mean they were easy to find.
The current type system with the [generalized subtype check][gsubck]
is my third attempt at writing a typechecker for RCL.

Sometimes you have a piece of code.
It’s not pretty, but it works.
Something feels off about it.
You can’t quite put your finger on it,
but it’s not something that a superficial refactor could fix.
With every new feature
the code grows a little more complex and intractable,
but that’s just how software matures, right?
And then one day
— and this has happened to me only a handful of times in my career —
you discover the _right_ way of doing things.
Suddenly everything becomes simple and elegant,
and the features fit in naturally.

The generalized subtype check was one of those cases.
I started with a typechecker that could only report a binary
“well-typed” vs. “type error”.
To handle gradual typing it had `Type::Dynamic`,
but it was a special case everywhere,
and the typechecker itself dealt with inserting runtime checks where needed.
This worked but it became a mess.

For the second attempt,
I tried to represent type expectations differently from inferred types,
also with error reporting in mind.
I got pretty far,
it even worked with covariant types like `List`
...
and then it completely failed
when I got to function types,
which are contravariant in their arguments.
Only after going through those iterations,
I thought of viewing types as sets,
and treating the subtype check as the subset ordering on sets.

TODO: Insert picture again.

With that everything worked out,
and the implementation became a lot more elegant.

## The typechecker

The main workhorse of the typechecker is this method
(slightly simplified for the sake of the post):

```rust
struct TypeChecker {
    env: Env<SourcedType>,
}

impl TypeChecker {
    pub fn check_expr(
        &mut self,
        expected: &SourcedType,
        expr_span: Span,
        expr: &mut Expr,
    ) -> Result<SourcedType>
}
```

The `TypeChecker` struct itself only carries an `Env`,
the environment that contains the type for every name in scope.
To check an expression, we pass in:

 * The expected type.
   In most cases this will be `Type::Any`,
   but in some cases a syntactic construct prescribes the type.
   For example,
   the condition in an if-else expression must be a boolean.
 * The span (source location) of the expression,
   used to highlight the offending code in case of a type error.
   <!--
   In RCL the span is stored out of band;
   it’s not a property of the expression itself.
   This means that in most places where expressions are used,
   there is also a span as adjacent argument or field.
   Perhaps this is bad
   (like passing in array lengths out of band instead of using a slice),
   though making `Expr` self-descriptive has other issues.
   -->
 * The expression itself.
   It is mutable, because the typechecker
   may wrap the expression in a runtime type check.

The typechecker returns the inferred type on success,
or an error in case of a static type error.
The implementation is basically a big match statement
that handles all expressions.
For example, these are the arms for an if-else expression,
or for a variable:

```rust
let expr_type = match expr {
    Expr::IfThenElse {
        condition_span,
        condition,
        body_then,
        body_else,
        span_then,
        span_else,
        ..
    } => {
        self.check_expr(type_bool_condition(), *condition_span, condition)?;

        let type_then = self.check_expr(expected, *span_then, body_then)?;
        let type_else = self.check_expr(expected, *span_else, body_else)?;

        // The inferred type is the join of the two sides, which may be
        // more specific than the requirement (which they satisfy).
        Typed::Type(type_then.join(&type_else))
    }

    Expr::Var { span, ident } => match self.env.lookup(ident) {
        None => return span.error("Unknown variable.").err(),
        Some(t) => t.is_subtype_of(expected).check(*span)?,
    },

    ...
};
```

The `Typed` enum helps to distinguish between the two cases the subtype check
can run into:

 * `Typed::Type(T)` means that we know statically that the type is `T`.
 * `Typed::Defer(T)` means that we need to insert a runtime check to verify that

## Sourced type

TODO: Write that we track source spans for types and it's helpful.

## To do

TODO: Mention that it took me three attempts, functions are tough.
TODO: Join impl does not have to be the theoretical best join, can approximate.
TODO: Question utility, e.g. with dict indexing.
TODO: Pretty-printer was helpful.
