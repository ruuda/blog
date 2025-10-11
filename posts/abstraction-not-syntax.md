---
title: Abstraction, not syntax
date: 2025-10-01
lang: en-US
synopsis: Bikeshed about configuration formats is beside the point. Alternative formats solve superficial problems, languages solve the real ones.
minutes: ?
run-in: The world is growing tired of yaml.
teaser: automating-configuration-updates
---

The world is growing tired of yaml.
Alternative configuration formats are making the rounds.
Toml has steadily been gaining traction,
in part due to tools like Cargo
and adoption in the Python standard library.
Json supersets
(with [comments][jsnc],
[commas][jwcc],
and [the digit 5][json5])
are flourishing,
while [KDL], [kson] and now [maml] promise to hit the sweet spot
between friendly and simple.

[jsnc]:  https://jsonc.org/
[jwcc]:  https://nigeltao.github.io/blog/2021/json-with-commas-comments.html
[json5]: https://json5.org/
[KDL]:   https://kdl.dev/
[kson]:  https://kson.org/
[maml]:  https://maml.dev/

While [I do believe that yaml is harmful][yamlhell],
all of the simpler formats are basically fine,
and their differences are mostly superficial.
The one real difference is in their data models.
Most formats adopt the json data model of objects and arrays,
while KDL, [HCL], and e.g. [Nginx][nginx] adopt
the XML data model of named nodes with attributes and children.
The rest is just syntax.
And yes, syntax _does_ matter,
but line noise is not the real problem here!

[nginx]:    https://nginx.org/en/docs/beginners_guide.html#conf_structure
[HCL]:      https://opentofu.org/docs/language/syntax/configuration/
[yamlhell]: /2023/01/11/the-yaml-document-from-hell

## Syntax is superficial

Let’s look at an example:
suppose we need to define cloud storage buckets to store backups.
We want to back up two databases: Alpha and Bravo.
For each we need three buckets:
one for hourly, daily, and monthly backups.
They should have a lifecycle policy
that deletes backups after 4, 30, and 365 days.
We don’t want to click around,
so we’ll set this up using an infrastructure-as-code tool
using the following hypothetical configuration file:

```json
{
  "buckets": [
    {
      "name": "alpha-hourly",
      "region": "eu-west",
      "lifecycle_policy": { "delete_after_seconds": 345600 }
    },
    {
      "name": "alpha-daily",
      "region": "eu-west",
      "lifecycle_policy": { "delete_after_seconds": 2592000 }
    },
    {
      "name": "alpha-monthly",
      "region": "eu-west",
      "lifecycle_policy": { "delete_after_seconds": 31536000 }
    },
    {
      "name": "bravo-hourly",
      "region": "us-west",
      "lifecycle_policy": { "delete_after_seconds": 345600 }
    },
    {
      "name": "bravo-daily",
      "region": "eu-west",
      "lifecycle_policy": { "delete_after_seconds": 259200 }
    },
    {
      "name": "bravo-monthly",
      "region": "eu-west",
      "lifecycle_policy": { "delete_after_seconds": 31536000 }
    }
  ]
}
```

Yes, this file would look friendlier in a different format.
But the file also contains two bugs,
and switching formats is not going to fix those.
Can you spot them?
To avoid spoilers,
here’s a bit of yaml to pad the page.
I’ll even throw in some comments for clarity:

```yaml
buckets:
  - name: "alpha-hourly"
    region: "eu-west"
    lifecycle_policy:
      delete_after_seconds: 345600  # 4 days
  - name: "alpha-daily"
    region: "eu-west"
    lifecycle_policy:
      delete_after_seconds: 2592000  # 30 days
  - name: "alpha-monthly"
    region: "eu-west"
    lifecycle_policy:
      delete_after_seconds: 31536000  # 365 days
  - name: "bravo-hourly"
    region: "us-west"
    lifecycle_policy:
      delete_after_seconds: 345600  # 4 days
  - name: "bravo-daily"
    region: "eu-west"
    lifecycle_policy:
      delete_after_seconds: 259200  # 30 days
  - name: "bravo-monthly"
    region: "eu-west"
    lifecycle_policy:
      delete_after_seconds: 31536000  # 365 days
```

What’s wrong?

 * `bravo-hourly` is located in the US,
   while the other buckets are in Europe.
 * `bravo-daily` is missing a zero on the expiration time,
   and keeps backups for only 3 days,
   instead of the intended 30.

Would you have caught those in review?
Now suppose we need to add a third database, Charlie.
We copy-paste the three stanzas,
and change `bravo` to `charlie`.
Congrats, we now copied the bugs!

Adopting a different format might make the file easier on the eye,
but it doesn’t reduce repetition,
and therefore it doesn’t address the real problem.

## Abstraction

While line noise matters to some extent,
the real problem is that we have no tools for abstraction.
We can bikeshed about quote styles and trailing commas,
but what we really need is a _for loop_.
This is what that same configuration looks like in [RCL]:

<pre><code class="sourceCode">{
  <span class="n">buckets</span> = [
    <span class="kw">let</span> period_retention_days = {
      <span class="n">hourly</span> = <span class="dv">4</span>,
      <span class="n">daily</span> = <span class="dv">30</span>,
      <span class="n">monthly</span> = <span class="dv">365</span>,
    };
    <span class="kw">for</span> database <span class="kw">in</span> [<span class="st">"alpha"</span>, <span class="st">"bravo"</span>]:
    <span class="kw">for</span> period, days <span class="kw">in</span> period_retention_days:
    {
      <span class="n">name</span> = <span class="st">f"</span><span class="dt">{</span>database<span class="dt">}</span><span class="st">-</span><span class="dt">{</span>period<span class="dt">}</span><span class="st">"</span>,
      <span class="n">region</span> = <span class="st">"eu-west"</span>,
      <span class="n">lifecycle_policy</span> = { <span class="n">delete_after_seconds</span> = days * <span class="dv">24</span> * <span class="dv">3600</span> },
    }
  ],
}
</code></pre>

It’s a bit more to take in at first,
but if you’ve ever seen Python, Rust or TypeScript,
you can read this file.
(For a more gentle introduction,
check out [the tutorial][rcl-tutorial].)
Note how we eliminated two categories of bugs,
and made the file more maintainable:

* We can’t mix up regions,
  because the region is only defined _once_.
* Instead of a comment that solemny swears
  that there are 31,536,000 seconds in 365 days,
  we have a _formula_ that computes this.
  A large magic number is meaningless,
  but most people will recognize 3600 as the number of seconds in an hour,
  and 24 as the number of hours in a day.
* If we ever need to add `charlie`, that’s a 1-line diff.

_This_ is where the real win is!

[RCL]:          https://rcl-lang.org/
[rcl-tutorial]: https://docs.ruuda.nl/rcl/tutorial/

## Trade-offs

While generating configuration solves problems,
it also creates new problems:

 * The applications we need to configure probably don’t
   natively accept config in your favorite language.
   This means we need to introduce
   an intermediate build step to generate the final configuation files.
   Features like [`rcl build`][rcl-build]
   help to reduce friction,
   but it’s still an additional step.
 * [Greppability] suffers.
   For example, the full names of the buckets
   no longer occur directly in the source code.
 * There is a fine line between tasteful abstraction and overcomplication.
   After how many lines of duplicated code
   does the power of a configuration language
   outweigh the simplicity of plain data?

When we have configuration files under source control,
we can mitigate these points somewhat
by checking in the generated files.
(Heresy, I know!)
This restores the ability to search,
and we can now review changes from both sides:
we can see the diff in the generator,
but also how it impacts generated files.

[rcl-build]:    https://docs.ruuda.nl/rcl/generating_files/
[Greppability]: https://morizbuesing.com/blog/greppability-code-metric/

## Tools

Configuration languages like [Cue], [Dhall], [Jsonnet], or [RCL]
are designed specifically to eliminate boilerplate in repetitive configuration,
but you don’t necessarily have to introduce new tools.
A bit of Python or Nix that outputs json or toml can go a long way.
Remember that yaml
<span
  style="text-decoration: underline dotted #c36; text-underline-offset: 2pt"
  title="As of yaml 1.2, but even if the target is different,
the pathological edge cases don’t matter in practice
when we control the generated json."
>is</span>
a json superset,
so anything that takes yaml accepts json!
Deduplication beats copy-pasting,
and manipulating data structures is safer than string templating.

[Dhall]:   https://dhall-lang.org/
[Jsonnet]: https://jsonnet.org/
[Cue]:     https://cuelang.org/

## Conclusion

The world is growing tired of yaml,
and alternative configuration formats are making the rounds.
While I applaud replacing yaml with simpler formats like toml,
and I prefer working with pretty code over working with ugly code,
I also think that arguing over which multi-line string syntax is superior,
is missing a deeper issue.

When configurations grow more complex,
what we really need is _abstraction_ to eliminate duplication.
Real abstraction over data structures,
not string templating.
That means turning configuration,
at least to some extent,
into code.
How to navigate the balance between code and data
remains a matter of applying good judgment,
but for large enough configurations,
the right balance is unlikely to be at 100% data.
