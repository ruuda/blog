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
It enables templating structured data.
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

In this post we’ll explore the trade-offs involved in adding this feature.

## Why unpack?

Unpack does not make RCL more expressive.
What unpack can do, was already possible with comprehensions.
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

## Dead ends

Before landing on the current implementation,
I struggled with two aspects.

 * I would like to use `..` for every unpack.
 * How should the typechecker deal with “things that can be unpacked”?

The complicating factor behind both is that RCL has sets,
in addition to lists and dicts.
As in Python,
both sets and dicts are written with curly braces.
This causes some implementation complexity,
but at least it was always possible to tell dicts and sets apart syntactically:
dicts contain key-value pairs,
whereas sets contain single values.
(The one exception is the empty collection `{}`,
but json compatibility forces this to be a dict.
The empty set is written `std.empty_set`.)
These are clear:

```rcl
let set1 = { 1, 2, 3 };
let set2 = { for x in set: x };

let dict1 = { a = 1, b = 2 };
let dict2 = { for k, v in  dict: k: v };
```

But if `..` unpacked both sets and dicts,
then what is this?

```rcl
let unknown = { ..xs };
```

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

The solution is to use a different syntax for dict unpack,
than for list/set unpack.
In one sense,
this makes RCL more complex:
there is now _more_ syntax,
_more_ constructs.
But in [the Hickeyan sense][simple-easy],
reusing `..` for two different purposes complects them,
and the separate `..` and `...` are each simple.
Something something many cases to report errors.

[simple-easy]: https://www.infoq.com/presentations/Simple-Made-Easy/

 * Sensible type system.
 * Syntactic distinction between dict and set.
 * `..` only.

## Do we really need sets?

Maybe ... not?

## Unpack in other languages

While unpack does not increase expressivity,
it greatly improves ergonomics.
I was pretty sure I wanted it in RCL,
and now that it exists,
it works well and feels natural.
This is no surprise:
unpack is not exactly a new idea.
Python is a big source of inspiration for RCL,
and it has unpack (written `*` and `**` rather than `..` and `...`).
Even Javascript has it nowadays (called ‘spread’ there).
Among other configuration languages though,
none of
Cue,
Dhall,
Jsonnet,
or Nickel feature unpack.
And then how to conclude this paragraph?

## Conclusion

Need to write a conclusion here.

[rcl-lang]: https://rcl-lang.org
[v0110]:    https://docs.ruuda.nl/rcl/changelog/#0110
