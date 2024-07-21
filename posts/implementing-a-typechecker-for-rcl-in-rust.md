---
title: A type system for RCL: Implementing a typechecker in Rust
header: A type system for RCL
subheader: Implementing a typeche<span class="dlig">ck</span>er in Rust
part: 4
lang: en-US
minutes: 14
date: 2024-07-21
synopsis: I am adding a type system to RCL, my configuration language. In part 4, we look at how the typechecker is implemented in Rust, and at how it is able to generate good error messages.
teaser: a-language-for-designing-slides
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
[rcl-lang]:  https://github.com/ruuda/rcl

 * [Part <abbr>I</abbr>: Introduction][part1]
 * [Part <abbr>II</abbr>: The type system][part2]
 * [Part <abbr>III</abbr>: Related work][part3]
 * [Part <abbr>IV</abbr>: The typechecker][part4] (this post)

[part1]: /2024/a-type-system-for-rcl-part-1-introduction
[part2]: /2024/a-type-system-for-rcl-part-2-the-type-system
[part3]: /2024/a-type-system-for-rcl-part-3-related-work
[part4]: /2024/implementing-a-typechecker-for-rcl-in-rust

[static]: /2024/a-type-system-for-rcl-part-1-introduction/#static-vs.-runtime
[gsubck]: /2024/a-type-system-for-rcl-part-2-the-type-system#the-generalized-subtype-check

In [part one][part1] we looked at what I want from a type system for RCL,
and in [part two][part2] we saw what the resulting gradual type system looks like.
The types form a lattice,
and typechecking involves a generalized notion of the subtype relationship.
In this part we’ll take a closer look
at how the typechecker is implemented in Rust.
After this post,
we will understand how RCL identifies the type error in this program,
and how it is able to localize and explain the error:

<pre><code class="sourceCode"><span class="kw">let</span> ports = [<span class="dv">22</span>, <span class="dv">80</span>, <span class="dv">443</span>];

<span class="co">// Use string keys so we can export as json.</span>
<span class="kw">let</span> firewall_rules: <span class="dt">Dict</span>[<span class="dt">String</span>, <span class="dt">String</span>] = {
  <span class="kw">for</span> port <span class="kw">in</span> ports:
  <span class="n">port</span>: <span class="st">"allow"</span>
};
</code></pre>

<pre><code class="sourceCode">  <span class="dt">|</span>
6 <span class="dt">|</span>   port: "allow"
  <span class="dt">|</span>   <span class="dt">^~~~</span>
<span class="dt">Error:</span> Type mismatch. Expected <span class="dt">String</span> but found <span class="dt">Int</span>.

  <span class="st">|</span>
4 <span class="st">|</span> let firewall_rules: Dict[String, String] = {
  <span class="st">|</span>                          <span class="st">^~~~~~</span>
<span class="st">Note:</span> Expected String because of this annotation.

  <span class="st">|</span>
1 <span class="st">|</span> let ports = [22, 80, 443];
  <span class="st">|</span>              <span class="st">^~</span>
<span class="st">Note:</span> Found Int because of this value.
</code></pre>

(One way to fix the error is to convert the integer to a string
using string interpolation,
by writing `f"{port}":` `"allow"` on line 6.)

## Failed attempts

Elegant ideas appear obvious in hindsight,
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
and the features fit in naturally without complicating each other.
The generalized subtype check was one of those cases.

I started with a typechecker that could only report a binary
“well-typed” vs. “type error”.
To handle gradual typing it had `Type::Dynamic`,
but it was a special case everywhere,
and the typechecker itself dealt with inserting runtime checks where needed.
This worked but it became a mess.

For the second attempt,
I tried to represent type expectations differently from inferred types,
in order to generate better error messages.
In hindsight,
checking that the inferred type met the expectation was a subtype check,
but I didn’t see it that clearly at first.
I got pretty far,
it even worked with covariant types like `List`
...
and then it completely failed
when I got to function types,
which are contravariant in their arguments.

At some point after going through those iterations,
it occurred to me to view types as sets of values.
Then the subset ordering on sets is the subtype relationship.
Then everything fell into place,
and the implementation became a lot more elegant.

## The evaluation pipeline

R<!---->C<!---->L evaluates a document in several stages:

 1. The lexer produces a sequence of tokens.
 2. The parser turns those into a _concrete syntax tree_ (CST).
    This tree preserves all syntactic constructs,
    including comments,
    blank lines,
    underscores in integer literals,
    etc.
    This tree is not only used for the next evaluation stage,
    it is also used by the autoformatter.
 3. The abstractor turns the CST into an _abstract syntax tree_ (AST).
    The AST drops comments and normalizes constructs.
    For instance, it has only one kind of integer literal,
    whereas the CST distinguishes between binary, decimal, and hexadecimal integer literals.
 4. The typechecker walks the AST,
    inferring and checking types on the fly,
    and inserting nodes for runtime checks where needed.
    Unlike more advanced type systems,
    RCL’s typechecker performs a single pass;
    it is not based on constraints that need to be solved later.
    The typechecker does not store most of the intermediate inferred types,
    they only exist for the sake of typechecking.
 5. The evaluator walks the AST again and evaluates the document into a value.
 6. Finally, the pretty-printer formats the value into an output to display.

In the remainder of this post,
we’ll take a closer look at step **4**.

## The typechecker

The main workhorse of the typechecker is the `check_expr` method.
It implements both type inference and typechecking:

```rust
struct TypeChecker {
    // Lifetimes simplified for the sake of this post.
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
   At the top level this is `Type::Any`.
   In most cases the expected type is passed down,
   but an expectation can also come from an annotation,
   and in some cases a syntactic construct prescribes the type.
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

The method returns the inferred type on success,
or an error in case of a static type error.
The implementation is a big match statement.
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
        // more specific than the requirement (which they both satisfy).
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
and if the check passes,
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

            // [Some match arms omitted in this snippet.]

            // If we take an arbitrary value, is it a member of some
            // type T, when T is not `Any` (that case is already
            // covered above)? We don't know statically.
            (Type::Any, _) => TypeDiff::Defer(other.clone()),

            // [More match arms omitted here.]

            // If we have any other combination of types,
            // they are incompatible.
            _ => TypeDiff::Error(Mismatch::Atom {
                actual: self.clone(),
                expected: other.clone(),
            }),
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
Sourced types are part of the error reporting machinery.
A sourced type is a pair of a type and its _source_:

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

[Like types][lattice], `Source` forms a lattice,
which in turn makes `SourcedType` a lattice.
When we join types for the sake of inference,
we also join their sources.
This is how for the example in the introduction,
the inferred type `List[Int]` for `ports`
can keep the `Int` pointing at one of the integer literals,
so it can explain in the error why `port` has type `Int`.
Let’s look at that example again:

<pre><code class="sourceCode"><span class="kw">let</span> ports = [<span class="dv">22</span>, <span class="dv">80</span>, <span class="dv">443</span>];

<span class="co">// Use string keys so we can export as json.</span>
<span class="kw">let</span> firewall_rules: <span class="dt">Dict</span>[<span class="dt">String</span>, <span class="dt">String</span>] = {
  <span class="kw">for</span> port <span class="kw">in</span> ports:
  <span class="n">port</span>: <span class="st">"allow"</span>
};
</code></pre>

<pre><code class="sourceCode">  <span class="dt">|</span>
6 <span class="dt">|</span>   port: "allow"
  <span class="dt">|</span>   <span class="dt">^~~~</span>
<span class="dt">Error:</span> Type mismatch. Expected <span class="dt">String</span> but found <span class="dt">Int</span>.

  <span class="st">|</span>
4 <span class="st">|</span> let firewall_rules: Dict[String, String] = {
  <span class="st">|</span>                          <span class="st">^~~~~~</span>
<span class="st">Note:</span> Expected String because of this annotation.

  <span class="st">|</span>
1 <span class="st">|</span> let ports = [22, 80, 443];
  <span class="st">|</span>              <span class="st">^~</span>
<span class="st">Note:</span> Found Int because of this value.
</code></pre>

Notice something else?
The error points at the dict key `ports` specifically.
We can do this because typechecking and inference are fused and top-down.
When the typechecker enters the dict comprehension,
it already has an expected type `Dict[String,` `String]`.
The dict comprehension satisfies the `Dict` part,
so inside the comprehension,
we have an expected type for the key and value (both `String`).

Compare this to an approach where we would infer types bottom-up,
and check compatibility afterwards.
Then the typechecker would infer `Dict[Int,` `String]` for the dict comprehension,
and it would have to report something like this:

<pre><code class="sourceCode">  <span class="dt">|</span>
4 <span class="dt">|</span> let firewall_rules: Dict[String, String] = {
  <span class="dt">|</span>                                            <span class="dt">^</span>
<span class="dt">Error:</span> Expected <span class="dt">Dict[String, String]</span> but found <span class="dt">Dict[Int, String]</span>.
</code></pre>

Now it’s up to the user to diff those types in their head,
and then search the source code for where the violation happens.
Pushing the expectation in top-down enables friendlier errors.
This example is maybe a bit contrived,
but it will make a big difference for record types,
where types can grow big.

[lattice]: /2024/a-type-system-for-rcl-part-2-the-type-system#the-type-lattice

## Incremental implementation

Having an `Any` type makes implementing type inference very forgiving.
If there is a case where things get too complex,
we can just return `Any`.
For example,
because record types are not yet implemented,
all field lookups currently infer as `Any`.
The more precise the inferred type,
the more errors we can catch statically,
but because we also have a runtime check,
returning `Any` is not incorrect.

There is one more place where we can cut some corners:
in the `join` implementation.
Mathematically `join` should return the least upper bound,
but in practice it’s fine if we return _some_ upper bound.
For example,
`join` does not produce `Union` if none of the inputs are:

<pre><code class="sourceCode"><span class="co">// If we annotate as List[Union[Int, Bool]] it typechecks,</span>
<span class="co">// but without annotation the inferred type is List[Any].</span>
<span class="kw">let</span> xs = [<span class="dv">42</span>, <span class="kw">true</span>];
</code></pre>

This is partially laziness on my end,
but also a matter of keeping the typechecker efficient,
and keeping type errors tractable.
If we inferred union types,
type errors would quickly grow enormous.

## Future work

Although the type system works well,
it is not yet useful for large configurations in the real world.
Three major features are needed for that:

 * **Record types.**
   By now I don’t expect they are hard to implement
   (function types already carry named arguments, which are similar),
   I just need to sit down and do it.
 * **Type aliases.**
   With record types,
   you don’t want to spell out the full type everywhere;
   you write it down once and then refer to it.
   I have a rough idea of what I want,
   but there are some unresolved questions.
   How should scoping work?
   The same as for expressions?
   Does that mean you can have local type definitions?
   Or should types only be allowed at the top level?
   Do they need to go before all expressions then,
   or can you mix let bindings and type definitions?
   Etc.
   I think this will be a matter of implementing _something_,
   getting a feel for it,
   and then iterating.
 * **Importing types across files.**
   Because every RCL document contains a single expression,
   it is very clear what importing files means on the value level.
   For types,
   I don’t think it is feasible to limit them to one exported type per file.
   The main blocker here is coming up with a syntax
   that is both legible and not completely hideous.

Aside from those,
there are less important features that I want to add:

 * String literal types, so you can use a union of string literals to model enums.
 * Quantification and type variables,
   to be able to give a more accurate type to functions such as `filter`.
   Currently it is [documented][list-filter] with a type variable,
   but in the implementation all type variables are just `Any`,
   so the typechecker loses track
   of the list element type after passing through `filter`.

Furthermore some parts are just not implemented,
like method lookups.
Fortunately the type system implementation is forgiving,
and RCL is usable and useful right now without them.

[list-filter]: https://docs.ruuda.nl/rcl/type_list/#filter

## Conclusion

R<!---->C<!---->L is a new configuration language
that aims to reduce configuration boilerplate
by extending json into a simple functional language
that enables abstraction and reuse.
I am adding support for type annotations and a typechecker to it,
and in this post we looked at how that typechecker is implemented.
We saw how the fused typecheck and inference, `check_expr`,
is the cornerstone of the typechecker,
with `is_subtype_of` implementing the generalized subtype check.
On the more practical side,
passing down an expected type enables
localizing type errors accurately,
and tracking the sources of types helps to explain type errors.

This post concludes my series about RCL’s type system.
One of RCL’s goals is to be obvious and easy to use
for people who have some background
in mainstream languages like Python or TypeScript,
but not necessarily typed functional programming.
As such,
the type system is relatively simple,
and mostly putting together existing ideas.
The main idea that I haven’t seen implemented elsewhere
is the generalized subtype check.
And to be fair,
it is of limited use
— the reason RCL can get away with it,
is [that runtime errors are static errors][static] in a configuration language.
I would love to hear from people more versed in the literature
if something like this exists in other systems,
and if there are common practices around it.

If this series got you interested in RCL,
[try RCL in your browser][rcl-playground],
and check out [the type system documentation][rcl-type-docs]!
I don’t recommend relying on RCL to generate production configuration yet:
it is a hobby project without statibility promise.
However, I do use it almost daily [as a `jq` replacement][rcl-jq],
and the new [map and filter methods in v0.4.0][rcl-v04]
make it even nicer for that.

[rcl-type-docs]:  https://docs.ruuda.nl/rcl/types/
[rcl-playground]: https://rcl-lang.org/#try-it-yourself
[rcl-jq]:         /2024/a-reasonable-configuration-language/#an-unexpected-jq-replacement
[rcl-v04]:        https://docs.ruuda.nl/rcl/changelog/#040
