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
while KDL (like [HCL], and e.g. [Nginx][nginx]) adopts
the XML data model of named nodes with attributes and children.
The rest is just syntax.
And yes, syntax _does_ matter,
but line noise is not the real problem here!

[nginx]:    https://nginx.org/en/docs/beginners_guide.html#conf_structure
[HCL]:      https://opentofu.org/docs/language/syntax/configuration/
[yamlhell]: /2023/01/11/the-yaml-document-from-hell

## Syntax is superficial

The real problem is the inability to do safe abstraction.
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

Yes, this file would be friendlier on the eye in a different format.
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
and therefore doesn’t address the real problem.

## Abstraction

While line noise matters to some extent,
the real problem is that we have no tools for abstraction.
We can bikeshed about quote styles and trailing commas,
but what we really need is a _programming language_.
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
* Instead of a comment that promises
  that there are usually 31,536,000 seconds in a year,
  we now have an obvious _formula_ that computes it.
* If we ever need to add `charlie`, that’s a 1-line diff.

_This_ is where the real win is!

[RCL]:          https://rcl-lang.org/
[rcl-tutorial]: https://docs.ruuda.nl/rcl/tutorial/

## Trade-offs

While generating configuration solves problems,
it also introduces new problems:

 * The applications we need to configure probably don’t
   natively accept config in your favorite language.
   This means we need to introduce
   an intermediate build step to generate the final configuation files.
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

[Greppability]: https://morizbuesing.com/blog/greppability-code-metric/

## Conclusion

How to navigate the balance between code and data
remains a matter of applying good judgment,
but for large enough configurations,
the right balance is unlikely to be at 100% data.

## Conclusion

Configuration languages such as [Cue], [Dhall], [Jsonnet], or [RCL]
are designed for this,
but you don’t necessarily have to introduce new tools.
A bit of Python or Nix that outputs json or toml can go a long way.
Deduplication beats copy-pasting,
and manipulating data structures is safer than string templating.

## Configuration languages

Let’s introduce some terminology.

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
[Dhall] has a rigid static type system,
[RCL] has a gradual type system,
and [Jsonnet] is dynamically typed.
All three are functional languages with user-defined functions.
[Cue] takes a very different approach,
but nonetheless enables abstraction.

[Dhall]:   https://dhall-lang.org/
[Jsonnet]: https://jsonnet.org/
[Cue]:     https://cuelang.org/

Abstracting configuration seems to be a bikeshed magnet.
After how many duplicated lines
does the power of a configuration language
outweigh the simplicity of plain data?
Is the syntax sufficiently lightweight?
Does it have enough stars on GitHub?
What about tooling?
Do we need a domain-specific language at all,
or can we just write some Python or Nix?
These are fair questions,
but my point is not to start a discussion
about tool preferences.
The point is to _start doing abstraction at all_.
Real abstraction, not string templating.

## How to configure your application

I’m advocating for configuration languages.
Does that mean that applications need to pick and embed one?
No!
We can have the best of all worlds!
Almost, at least.

 * We can have small hand-written configurations in a friendly format.
 * We can generate larger configurations using your favorite tool.

Here’s how:

 1. Define configuration as a data structure in your program.
 2. Ensure you can deserialize the data structure from json and toml,
    and if you like, yaml or a json dialect.

Now we can start simple,
and write a toml file by hand.
Once the configuration grows more complex,
or we need to configure multiple similar instances,
we can generate configuration using any tool
— whether that’s a configuration language, Nix, or just Python —
and export that as json.
Do you prefer a different syntax?
Sure, use your favorite tool, and export to json.
If we put generated files under source control along with their source
(heresy, I know)
they even remain greppable,
and the full impact of changes can be inspected and reviewed.

This is easy to do.
For example, in Rust we get deserialization from json and toml
almost for free with a few Serde derive annotations.
In Python we have dataclasses, json, and toml right in the standard library,
and Pydantic to make it easier to work with nested types.
In about 10 lines of code,
you can accept both json and toml.

Applications don’t need to force
a configuration language or exotic format onto users;
json is already the universal data format.

Todo meh something something build step.

## Conclusion

Configuration format is not configuration language.

## Appendix: Example

In Rust, we can use `serde`, `serde_json`, and `toml`:

```rust
#[derive(Debug, Deserialize)]
struct Config {
    app: AppConfig,
    server: ServerConfig,
    database: DatabaseConfig,
}

#[derive(Debug, Deserialize)]
struct ServerConfig {
    /// The address and port to listen on, e.g. `127.0.0.1:8000`.
    listen: String,

    /// The number of http handler threads to start.
    num_threads: u32,
}

// Other structs omitted for brevity.

fn load_config(fname: &Path) -> Result<Config> {
    let data = std::fs::read_to_string(fname)?;
    let config = match () {
      _ if fname.ends_with(".toml") => toml::from_str(&data)?,
      _ if fname.ends_with(".json") => serde_json::from_str(&data)?,
      _ => return Err(anyhow!("Unrecognized config format.")),
    };
    Ok(config)
}
```

In Python we can use `pydantic`, `json`, and `tomllib`:

```python
class Config(BaseModel):
    app: AppConfig
    server: ServerConfig
    database: DatabaseConfig


class ServerConfig(BaseModel):
    # The address and port to listen on, e.g. `127.0.0.1:8000`.
    listen: str

    # The number of http handler threads to start.
    num_threads: int


def load_config(fname: str) -> Config:
    data_str = open(fname, "r", encoding="utf-8").read()

    if fname.endswith(".toml"):
        data = tomllib.loads(data_str)
    elif fname.endswith(".json"):
        data = json.loads(data_str)
    else:
        raise Exception("Unexpected config format.")

    return Config(**data)
```
