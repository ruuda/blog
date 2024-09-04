---
title: The numeric type alignment chart
date: 2024-09-01
lang: en-US
minutes: ?
synopsis: TODO
run-in: I am building
---

I am building a new configuration language, [RCL][rcl-lang].
It’s a superset of json
and [it features a gradual type system][types].
Its main purpose is to generate json, yaml, and toml files,
but it makes a pretty good json query language too.
(Think `jq`, but without having to ask ChatGPT to write the query for you.)
It supported integers early on,
but to deliver on the json superset promise,
the only piece that is still missing are floats;
numbers that contain a decimal point or exponent.
Adding those turns out to be a rabbit hole of trade-offs.
<!-- TODO: Mention sets. -->
The following alignment chart summarizes this challenge:

[rcl-lang]: https://rcl-lang.org
[types]:    /2024/a-type-system-for-rcl-part-1-introduction

<div style="overflow-x: auto">
<div style="overflow: hidden; width: fit-content">
<table style="border-spacing: 1em; margin: 0 -1em 1em -1em; min-width: 30em">
<thead>
<tr>
  <td></td>
  <td>
    <strong>Value purist</strong>
    <br>Value identity must respect numeric equality.
  </td>
  <td>
    <strong>Value neutral</strong>
    <br>A decimal point is part of a number’s identity.
  </td>
  <td>
    <strong>Value rebel</strong>
    <br>Formatting is part of a number’s identity.</td>
  </td>
</tr>
</thead>
<tbody>
<tr>
  <td>
    <strong>Type purist</strong>
    <br>A number is either an int or a float.
  </td>
  <td><code>1 == 1.0</code> and <code>{1, 1.0}</code> are type errors.</td>
  <td><code>1 ≠ 1.0</code>,<br>just like<br><code>1 ≠ "1"</code>.</td>
  <td><code>1.0 ≠ 1.00</code>,<br>just like<br><code>"1.0" ≠ "1.00"</code>.</td>
</tr>
<tr>
  <td>
    <strong>Type neutral</strong>
    <br>Int is a subtype of float.
  </td>
  <td><code>{1, 1.0}</code> has one element.<br><code>1.0</code> is an int.</td>
  <td><code>{1, 1.0}</code> has two elements.<br><code>1</code> is an int.</td>
  <td>Ints and floats are strings that match regexes.</td>
</tr>
<tr>
  <td>
    <strong>Type rebel</strong>
    <br>All numbers are floats.
  </td>
  <td><code>{1, 1.0}</code> has one element.<br><code>1</code> is a float.</td>
  <td><code>{1, 1.0}</code> has two elements.<br><code>1</code> is a float.</td>
  <td>Numbers are strings that match a regex.</td>
</tr>
</tbody>
</table>
</div>
</div>

Before we dive in to what this means,
let’s look at where the challenge comes from.

## Json semantics

The json specification is a _lexical_ one:
it prescribes what strings are valid json documents,
but it does not prescribe the _semantics_ of those documents.
It’s up to the application to interpret the document.

In some cases,
all sane applications agree on the semantics.
For example,
whitespace is insignificant,
and `"?"`, `"\u005f"`, and `"\u005F"` all represent the same string
(the code point <abbr>U+005F</abbr>, a question mark).
For numbers, it’s not that clear.
I think it’s reasonable to expect `1.0` and `1.00` to be interchangeable,
but what about `1` and `1.0`?
Some applications reject numbers with a decimal point
when they expect an integer.
The converse — _demanding_ a decimal point when expecting a float —
is less common,
but still something that an application might reasonably do.

Because RCL aims to generate configuration
for any application that accepts json, yaml, or toml,
it can’t assume that the presence or absence of a decimal point is irrelevant.
I think it is fair to assume that `1.0` and `1.00` are interchangeable
— at some point you have to be pragmatic,
and weigh usability above support for applications with uncommon behavior.
But distinguishing between ints and floats is pretty common,
so RCL should definitely never insert decimal points,
and ideally it shouldn’t remove them either.
