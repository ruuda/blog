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

The main workhorse of the typechecker is the `check_expr` method:

```rust
struct TypeChecker {
    // [Lifetimes simplified for the sake of this post.]
    env: Env<SourcedType>,
}

impl TypeChecker {
    pub fn check_expr(
        &mut self,
        expected: &SourcedType,
        expr_span: Span,
        expr: &mut Expr,
    ) -> Result<SourcedType> {
        // We'll see some of the implementation below.
    }
}
```

The `TypeChecker` struct itself only carries an `Env`,
the environment that contains the type for every name in scope.
To check an expression, we pass in:

 * **The expected type.**
   In most cases this will be `Type::Any`,
   but in some cases a syntactic construct prescribes the type.
   For example,
   the condition in an if-else expression must be a boolean.
 * **The span (source location) of the expression.**
   This is used to highlight the offending code in case of a type error.
   <!--
   In RCL the span is stored out of band;
   it’s not a property of the expression itself.
   This means that in most places where expressions are used,
   there is also a span as adjacent argument or field.
   Perhaps this is bad
   (like passing in array lengths out of band instead of using a slice),
   though making `Expr` self-descriptive has other issues.
   -->
 * **The expression itself.**
   It is mutable, because the typechecker
   may wrap the expression in a runtime type check.

The typechecker returns the inferred type on success,
or an error in case of a static type error.
The implementation is basically a big match statement
that matches on the expression.
For example, this is the check for an if-else expression:

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

    // [Other match arms omitted in this snippet.]
};
```

At the end, the typechecker inserts a runtime check if needed:

```rust
match expr_type {
    // If the type check passed, great, we now know the inferred type.
    Typed::Type(t) => Ok(t),

    // If we couldn't check statically, then we have to insert a runtime
    // type check around this node. We have to sacrifice a temporary
    // NullLit to the borrow checker to swap the node into place.
    Typed::Defer(t) => {
        let mut tmp = Expr::NullLit;
        std::mem::swap(&mut tmp, expr);
        *expr = Expr::CheckType {
            span: expr_span,
            type_: expected.clone(),
            body: Box::new(tmp),
        };
        Ok(t)
    }
}
```

The `Typed` enum helps to distinguish
between the two cases that the subtype check can run into.
`Type(T)` means that we know statically that the type is `T`;
`Defer(T)` means that we need to insert a runtime check,
but if the check passes,
then we have a value of type `T`.

The second workhorse of the typechecker is `is_subtype_of`.
It is used for example when checking literals
or variable lookups:

```rust
let expr_type = match expr {
    Expr::NullLit => {
        type_literal(expr_span, Type::Null)
            .is_subtype_of(expected)
            .check(expr_span)?
    }
    Expr::BoolLit(..) => {
        type_literal(expr_span, Type::Bool)
            .is_subtype_of(expected)
            .check(expr_span)?
    }
    Expr::StringLit(..) => {
        type_literal(expr_span, Type::String)
            .is_subtype_of(expected)
            .check(expr_span)?
    }
    Expr::Var { span, ident } => match self.env.lookup(ident) {
        None => return span.error("Unknown variable.").err(),
        Some(t) => t.is_subtype_of(expected).check(*span)?,
    }
    // [Other match arms omitted in this snippet.]
};
```

The `is_subtype_of` method is defined as follows:

```rust
impl SourcedType {
    pub fn is_subtype_of(
        &self,
        other: &SourcedType,
    ) -> TypeDiff<SourcedType> {
        match (&self.type_, &other.type_) {
            // Void is a subtype of everything, Any a supertype
            // of everything. They are the bottom and top of the
            // lattice.
            (Type::Void, _) => TypeDiff::Ok(self.clone()),
            (_, Type::Any) => TypeDiff::Ok(self.clone()),

            // [Other match arms omitted in this snippet.]
        }
    }
}
```

It returns a `TypeDiff`, whis is similar to `Result<Typed>`:

```rust
/// The result of a subtype check `T ≤ U`
/// where `U` is expected and `T` encountered.
pub enum TypeDiff<T> {
    /// Yes, `T ≤ U`, and here is `T`.
    Ok(T),

    /// For `t: T`, we *might* have `t: U`.
    /// Here is `V` such that `T ≤ V ≤ U`.
    Defer(T),

    /// For all `t: T`, we have that `t` is not a value of `U`.
    ///
    /// Or, in some cases this is not strictly true, but we want to
    /// rule out that case because it makes more sense. For example,
    /// we say that `List[Int]` and `List[String]` are incompatible,
    /// even though `[]` inhabits both.
    Error(Mismatch),
}
```

The `check` method on `TypeDiff` turns a `TypeDiff` into `Result<Typed>`.
It formats the structured `Mismatch` into a printable error.

## Sourced types

You might have noticed in the snippets above,
most places use `SourcedType` rather than `Type`.
This is used to report helpful errors.
A sourced type is just a combination of a type and its _source_:

```rust
pub struct SourcedType {
    pub type_: Type,
    pub source: Source,
}
```

A source describes where the type came from.
It contains cases like these:

```rust
pub enum Source {
    /// There is no source for this type.
    ///
    /// This is the case for `Type::Any` when it is introduced by the
    /// typechecker for expressions that are not otherwise constrained.
    None,

    /// The type was inferred from a literal.
    Literal(Span),

    /// It was a type annotation in the source code.
    Annotation(Span),

    /// A boolean is required, because it's used as a condition
    /// in `if` or `assert`.
    Condition,

    /// The type is the required type of the operator at the span.
    Operator(Span),

    /// An integer is required due to indexing into a list.
    IndexList,
}
```

R<!---->C<!---->L uses these sources when reporting a type error.
For example, in the program below we have `Source::Literal` and
`Source::Annotation`:

<pre><code class="sourceCode"><span class="kw">let</span> xs = [<span class="dv">42</span>, <span class="dv">43</span>, <span class="dv">44</span>];
<span class="kw">let</span> y: <span class="dt">Dict</span>[<span class="dt">Int</span>, <span class="dt">String</span>] = { <span class="dv">0</span>: xs[<span class="dv">0</span>] };
</code></pre>

<pre><code class="sourceCode">  <span class="dt">|</span>
2 <span class="dt">|</span> let y: Dict[Int, String] = { 0: xs[0] };
  <span class="dt">|</span>                                 <span class="dt">^~~~~</span>
<span class="dt">Error:</span> Type mismatch. Expected <span class="dt">String</span> but found <span class="dt">Int</span>.

  <span class="st">|</span>
2 <span class="st">|</span> let y: Dict[Int, String] = { 0: xs[0] };
  <span class="st">|</span>                  <span class="st">^~~~~~</span>
<span class="st">Note:</span> Expected String because of this annotation.

  <span class="st">|</span>
1 <span class="st">|</span> let xs = [42, 43, 44];
  <span class="st">|</span>           <span class="st">^~</span>
<span class="st">Note:</span> Found Int because of this value.
</code></pre>

TODO: Turn this around. Start with this example as a teaser.

Two things are worth highlighting here:

 * Like types, `Source` forms a lattice,
   which in turn makes `SourcedType` a lattice.
   When we join types for the sake of inference,
   we also join their sources.
   This is how the inferred type of `xs`, `List[Int]`,
   can keep the `Int` pointing at one of the integer literals.
 * Having an expected type passed in top-down
   is what enables blaming the error on the expression `xs[0]`.
   If we did full inference first,
   and only at the end checked that the inferred type matches the annotation,
   then the error would be much larger.
   We’d have to report that `Dict[Int,` `Int]`
   does not match the expected type `Dict[Int,` `String]`,
   and users would have to diff that type in their head
   to try and find the source of the problem.

This example is maybe a bit contrived,
but I expect that this will make a big difference for record types,
where types can grow big.

## To do

TODO: Mention that it took me three attempts, functions are tough.
TODO: Join impl does not have to be the theoretical best join, can approximate.
TODO: Question utility, e.g. with dict indexing.
TODO: Pretty-printer was helpful.
