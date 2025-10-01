---
title: Don’t confuse configuration formats with configuration languages
date: 2025-10-01
lang: en-US
synopsis: Bikeshed about configuration formats is beside the point. Alternative formats solve superficial problems, languages solve the real ones.
minutes: ?
run-in: The world is growing tired of yaml.
teaser: automating-configuration-updates
---

The world is growing tired of yaml.
Alternative configuration formats are making the rounds.
Toml has been steadily gaining ground,
in part due to tools like Cargo
and adoption in the Python standard library.
Json supersets
(with [comments][jcma],
[commas][jwcc],
and [the digit 5][json5])
are flourishing,
while [KDL], [kson] and now [maml] promise to hit the sweet spot
between friendly and simple.

[jcma]:  https://code.visualstudio.com/docs/languages/json#_json-with-comments
[jwcc]:  https://nigeltao.github.io/blog/2021/json-with-commas-comments.html
[json5]: https://json5.org/
[KDL]:   https://kdl.dev/
[kson]:  https://kson.org/
[maml]:  https://maml.dev/

While [I do believe that yaml is harmful][yamlhell],
any other format is basically fine,
and their differences are mostly superficial.
The one real difference is in their data models.
Most formats adopt the json data model of objects and arrays,
while KDL (like [HCL], and e.g. [Nginx][nginx] config) adopts
the XML data model of named nodes with attributes and children.
The rest is just syntax.
And yes, syntax _does_ matter,
but it doesn’t matter _that_ much.
Line noise is not the real problem here!

[nginx]:    https://nginx.org/en/docs/beginners_guide.html#conf_structure
[HCL]:      https://opentofu.org/docs/language/syntax/configuration/
[yamlhell]: /2023/01/11/the-yaml-document-from-hell

## Abstraction, not syntax

To give a concrete example of a more important problem,
suppose we need to define cloud storage buckets to store backups.
We want to back up two databases: Alpha and Bravo.
For both of them we need three buckets:
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

Sure, this file would be friendlier on the eye in a different format.
But the file also contains two bugs,
and switching formats is not going to catch those.
Can you spot them?
To avoid spoilers,
here’s some yaml to pad the page.
I’ll even throw in a few comments for clarity:

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

What's wrong?

 * `bravo-hourly` is located in the US,
   while the other buckets are in the EU.
 * `bravo-daily` is missing a zero on the expiration time,
   and keeps backups for only 3 days,
   instead of the intended 30.

Would you have caught those in review?

It gets worse:
suppose we need to add a third database, Charlie.
A perfect task for the intern,
who is going to copy three stanzas
and change `bravo` to `charlie`.
Congrats, we now copied the bugs!

While line noise matters,
the real problem is that we have no tools for abstraction.
We can bikeshed about formats,
but what we really need is a _language_.
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
We can’t mix up regions,
because the region is only defined _once_.
Instead of a comment that promises
that there are usually 31536000 seconds in a year,
we now have a _formula_ that computes it.
And if we need to add `charlie`,
that’s a 1-line diff.

[RCL]:          https://rcl-lang.org/
[rcl-tutorial]: https://docs.ruuda.nl/rcl/tutorial/

## Configuration languages

Let’s clarify the terminology:

* A **configuration format** specifies _data_,
  with limited or no abilities for abstraction.
* A **configuration language** is a
  domain-specific programming language
  optimized for expressing repetitive configurations.
  It’s _code_ as much as it is data.

As with formats,
there is no shortage of configuration languages.
Because there is more to a full language than there is to a format,
their differences are less superficial.
For example,
[Dhall] has a static type system,
RCL has a gradual type system,
and [Jsonnet] is dynamically typed.
All three are functional languages with user-defined functions.
[Cue] takes a very different approach,
but nonetheless enables abstraction.

[Dhall]:   https://dhall-lang.org/
[Jsonnet]: https://jsonnet.org/
[Cue]:     https://cuelang.org/

Configuration languages are a bikeshed magnet.
Which syntax looks more pleasant?
Which language has better tooling?
Does it have enough stars on GitHub?
After how many duplicated lines
does the power of a configuration language
outweigh the simplicity of plain data?
Do we need a new language at all,
or can we just write some Python or Nix?
I don’t want to spark that discussion right now,
that’s beside the point.
The point is to start doing abstraction at all.
_Real_ abstraction, not string templating.

## How to configure your applications

Foobar

 * Deserialize struct.
 * Accept toml or json, and if you like, yaml or a json dialect.
 * Check in the end result, so we can grep.

## Conclusion

Configuration format is not configuration language.
