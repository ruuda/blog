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
In [v0.11.0][v0110] I finally implemented this feature,
and you can now use `..` and `...` to unpack lists and dicts:

<pre><code class="sourceCode"><span class="kw">let</span> xs = [<span class="dv">3</span>, <span class="dv">4</span>];
<span class="kw">let</span> ys = [<span class="dv">1</span>, <span class="dv">2</span>, ..xs, <span class="dv">5</span>, <span class="dv">6</span>];

<span class="co">// TODO: Less contrived example.</span>
<span class="kw">let</span> defaults = { <span class="n">kind</span> = <span class="st">"fruit"</span>, <span class="n">tasty</span> = <span class="kw">true</span> };
<span class="kw">let</span> fruits = [
  { ...defaults, <span class="n">name</span> = <span class="st">"banana"</span> },
  { ...defaults, <span class="n">name</span> = <span class="st">"grapefruit"</span>, <span class="n">tasty</span> = <span class="kw">false</span> },
];
</code></pre>

In this post we’ll explore the trade-offs involved in adding this feature.

## Why unpack?

Unpack does not add new technical capabilities to RCL.
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
but it is one of those difference between a tool
that technically does what you need,
and one that’s a joy to use.
Moreover,
I expect the unpack version to be more self-explanatory to newcomers.
Aside from the formatting challenge,
the union operator has a fairly complex implementation in the type system,
and removing it would be a welcome simplification.

Unpack solves all these problems neatly,
so for a long time it was clear to me that RCL needed unpack.
Why did it take so long to add?

## Wishlist

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
