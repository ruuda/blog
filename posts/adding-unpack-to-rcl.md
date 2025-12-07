---
title: Adding unpack syntax to RCL
break: unpack syntax
date: 2025-11-13
lang: en-US
minutes: ??
synopsis: ??
run-in: I am building
---

I am building a new configuration language
and json query tool: [RCL](https://rcl-lang.org).
It extends json into a simple functional language
that enables abstraction and reuse.
Rather than string templating serialized data,
RCL enables you to template data structures directly.
A common operation here is to build lists and dicts
out of other lists and dicts.
While RCL had several ways to do this,
I wasn’t satisfied with them.
I wanted _unpack_.
In [v0.11.0][v0110] I finally implemented this feature,
and you can now use `..` and `...` to unpack lists and dicts:

<pre><code class="sourceCode"><span class="kw">let</span> xs = [<span class="dv">3</span>, <span class="dv">4</span>];
<span class="kw">let</span> ys = [<span class="dv">1</span>, <span class="dv">2</span>, ..xs, <span class="dv">5</span>, <span class="dv">6</span>];

<span class="kw">let</span> defaults = { <span class="n">kind</span> = <span class="st">"fruit"</span>, <span class="n">tasty</span> = <span class="kw">true</span> };
<span class="kw">let</span> fruits = [
  { ...defaults, <span class="n">name</span> = <span class="st">"banana"</span> },
  { ...defaults, <span class="n">name</span> = <span class="st">"grapefruit"</span>, <span class="n">tasty</span> = <span class="kw">false</span> },
];
</code></pre>

In this post we’ll explore the trade-offs involved in designing this feature.

## Why unpack?

Unpack does not make RCL more expressive.
Anything unpack can do, was already possible with comprehensions.
This list unpack and comprehension are equivalent:

<pre><code class="sourceCode">[<span class="dv">1</span>, <span class="dv">2</span>, ..xs]
[<span class="dv">1</span>, <span class="dv">2</span>, <span class="kw">for</span> x <span class="kw">in</span> xs: x]
</code></pre>


And this dict unpack and comprehension are equivalent:

<pre><code class="sourceCode">{ <span class="n">id</span> = <span class="dv">42</span>, ...opts }
{ <span class="n">id</span> = <span class="dv">42</span>, <span class="kw">for</span> k, v <span class="kw">in</span> opts: <span class="n">k</span>: v }
</code></pre>

Furthermore, the union operator `|`
could be used for dict and set unions.
With that, the above dict could be written as:

<pre><code class="sourceCode">{ <span class="n">id</span> = <span class="dv">42</span> } | opts
</code></pre>

There are two problems with those options.

 * Comprehensions are too verbose.
 * Binary operators are awkward to format.

The comprehensions aren’t even _that_ verbose,
but it’s enough friction that I dreaded writing them out every time,
and they obscure a simple operation
(splice a list, set, or dict into another one)
behind syntactic noise
(keywords, punctuation, and additional variables).
Even the triple dot is a bit verbose for my taste,
but we’ll get to why it exists below.

The union operator doesn’t suffer from verbosity,
but there is no great way to format it when one of the sides is a multi-line dict,
and I don’t like how in a larger multi-line union
the first term looks different from the others.
Once we express unions with unpack,
everything becomes completely uniform.
Compare:

<pre><code class="sourceCode"><span class="co">// With union operator:</span>
<span class="kw">let</span> widget =
  widget_default_opts
  | turbo_encabulator_opts
  | {
    <span class="n">id</span> = <span class="dv">42</span>,
    <span class="n">bearings</span> = <span class="st">"spurving"</span>,
  };

<span class="co">// With unpack:</span>
<span class="kw">let</span> widget = {
  ...widget_default_opts,
  ...turbo_encabulator_opts,
  <span class="n">id</span> = <span class="dv">42</span>,
  <span class="n">bearings</span> = <span class="st">"spurving"</span>,
};
</code></pre>

The difference is superficial,
but it is one of those differences between a tool
that technically does what you need,
and one that’s a joy to use.
Moreover,
I expect the unpack version to be more self-explanatory to newcomers.
Aside from the formatting challenge,
the union operator has a fairly complex implementation in the type system,
and removing it would be a welcome simplification.

Unpack solves these problems with a single mechanism.
It makes RCL more coherent,
and more pleasant to read.
For a long time it was clear to me that RCL needed unpack.
Why did it take so long to add?

## Meet Set, the troublemaker

It turns out that [as with number types][float],
my wishlist had incompatible items,
and one of them had to go.
I wanted:

 * A set data type.
 * That is written with curly braces just like dicts.
 * That is always syntactically distinguishable from dicts.
 * A single syntax, `..`, for all types of unpack.

It turns out, sets cause trouble.
As in Python,
both sets and dicts are written with curly braces in RCL.
This causes some implementation complexity,
but at least it was always possible to tell dicts and sets apart syntactically:
dicts contain key-value pairs,
whereas sets contain single values.
(The one exception is the empty collection `{}`,
which for json compatibility has to be a dict.
The empty set is written `std.empty_set`.)
These are unambiguous:

<pre><code class="sourceCode"><span class="kw">let</span> set1 = {<span class="dv">1</span>, <span class="dv">2</span>, <span class="dv">3</span>};
<span class="kw">let</span> set2 = {<span class="kw">for</span> x <span class="kw">in</span> xs: x};

<span class="kw">let</span> dict1 = { <span class="n">a</span> = <span class="dv">1</span>, <span class="n">b</span> = <span class="dv">2</span> };
<span class="kw">let</span> dict2 = { <span class="kw">for</span> k, v <span class="kw">in</span> dict: <span class="n">k</span>: v };
</code></pre>


But if `..` unpacked both sets and dicts,
then what is this?

<pre><code class="sourceCode"><span class="kw">let</span> unknown = { ..xs };
</code></pre>

It depends on whether `xs` is a dict or set,
we can’t tell from just the syntax tree.
It would be possible to deal with this at runtime
and in [the typechecker][typechecker],
but RCL aims to be a _reasonable_ configuration language,
and one thing that means to me is that you can reason about what a program does,
ideally without having to consult
the definitions of variables that may be far away.

[float]:       /2025/a-float-walks-into-a-gradual-type-system
[typechecker]: /2024/a-type-system-for-rcl-part-2-the-type-system

And so the wishlist items are incompatible.
One of them has to go.

**Removing sets.**<br>
Without sets,
all these problems go away.
Many other problems _also_ go away:
duplication between list and set methods,
having to write “list or set” in places that accept both,
and having only bad options for typing such cases
(unions exist but are verbose and may be confusing to newcomers,
but a dedicated collection type brings even more complexity).
Do we really need sets?
As with unsigned integers,
they fill a niche where they encode constraints that are sometimes useful,
but in priciple we could just use lists
and add e.g. a `unique` method that removes duplicates.
If your collections are so large that algorithmic complexity matters,
RCL is probably not the right language for your problem anyway.
So I tried it.
I deleted sets.
I played around with the stripped-down version,
but ultimately,
sets _are_ useful,
and I didn’t want to give up on them yet.

**Give sets a different syntax.**<br>
Using a single unpack syntax only creates an ambiguity
when dicts and sets are both written with curly braces.
What if sets used different symbols?
The problem here is that there aren’t that many symmetric pairs in ASCII.
`()`, `[]`, and `{}` are already in use,
and `<>` create ambiguities with the comparison operators.
(This is what makes C++ notoriously difficult to parse,
and it’s why Rust features the [turbofish].)
We could go for a digraph,
maybe `{||}`, `@{}`, or a keyword like `set {}`,
but they add visual noise,
and are less obvious to newcomers.
To me,
being _reasonable_ also means avoiding surprise,
respecting established conventions,
and being readable even to people who haven’t seen the language before.
The braces have to stay.

[turbofish]: https://doc.rust-lang.org/reference/glossary.html#turbofish

**Embrace ambiguity.**<br>
Is it really so bad
that we can’t tell whether `{ ..xs }` is a dict or set?
After all,
we can’t tell the type of just `xs` from the syntax tree either.
Even if it’s not clear from the syntax,
the typechecker can usually infer it,
and otherwise we can deal with it at runtime.
I did not try implementing this option,
partly because I was afraid it would be an invasive change,
and partly because I feel
the type of collection literals _should_ be obvious from a glance at the code.
I might revisit this option later.
We can always desugar `...` to `..` in a future version,
and the formatter could automatically upgrade documents.
The other direction
— going from one kind of unpack to two
— would be much harder.

**Use `...` for dict unpack.**</br>
This is what I settled on for now:
use `..` to unpack lists and sets,
and `...` to unpack dicts.
There is precedent for such a distinction:
Python uses `*` and `**`.
Having just one type of unpack is in one sense simpler:
there is less syntax to learn and memorize.
It’s also more discoverable:
what you know about list unpack
transfers to dicts.
However,
reusing `..` for both kinds of unpack
is _not_ simpler in [the Hickeyan sense][simple-easy],
and we can address discoverability
with helpful error messages.
Those turned out to be more complex than I expected
because of the many possible cases,
but in the end I handled them all,
and so far I’m happy with the result.

[simple-easy]: https://www.infoq.com/presentations/Simple-Made-Easy/

## Conclusion

R<!---->C<!---->L is a new configuration language and json query tool
that extends json into a simple functional language
that enables abstraction and reuse.
Common operations in RCL documents
are to splice one list into another,
to fill a dict with default values,
or to take the union of multiple sets.
While RCL supported this through comprehensions,
for simple cases they were too verbose to be ergonomic.
The union operator does not suffer from verbosity,
but its meaning is less obvious to newcomers,
and it can be awkward to format.
Unpack solves both problems elegantly,
and is now available in [RCL v0.11.0][v0110].

If this post piqued your interest,
try out RCL in [the online playgroud][play],
or jump straight to [the manual][manual].

[rcl-lang]: https://rcl-lang.org
[v0110]:    https://docs.ruuda.nl/rcl/changelog/#0110
[play]:     https://rcl-lang.org/#try-it-yourself
[manual]:   https://docs.ruuda.nl/rcl/
