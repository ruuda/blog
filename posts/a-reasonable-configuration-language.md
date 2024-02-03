---
title: A reasonable configuration language
date: 2024-02-03
lang: en-US
minutes: ??
synopsis: TODO
run-in: About six months ago
---

About six months ago, I was fed up with it.
The particular _it_ was HCL — Hashicorp Configuration Language —
but that was only the trigger,
it was hardly the only offender.
The issue I was struggling with that day
was to define six cloud storage buckets in Terraform.
They were similar, but not quite identical.
The kind of thing you’d do with [a two-line nested loop][rcl-loop]
in any general-purpose language,
but where all the ways of achieving that in HCL were so much hassle,
that is was far simpler to just copy-paste the config six times.

Although this HCL episode was the droplet,
my bucket of frustration had been filling up
for a longer time:

 * **GitHub Actions workflows** that differ in only a few commands
   — there is no native way to abstract those.
   The same often applies to jobs within a workflow.
 * **Kubernetes manifests** that are 80% the same for most applications,
   and an entire industry that fails to adopt a proper solution for this,
   and instead resorts to templating yaml,
   which to me is [very obviously very wrong on so many levels][template-yaml].
 * **The prevalence of yaml in general**,
   a format that does solve some problems
   (adding comments and a lighter syntax to json),
   but in the process introduces [so many new problems][yaml-hell]
   that the cure is arguably worse than the disease.
 * **Ansible playbooks** that are too similar to copy-paste,
   but different enough that parametrizing over data is insufficient.
   Related to this,
   the parameter data is difficult to share between Ansible and other tools.

[rcl-loop]: https://github.com/ruuda/rcl/blob/bedbd3eea1129ba6053427d67b77a955240ceca8/examples/buckets.rcl#L9-L10
[template-yaml]: /2023/01/11/the-yaml-document-from-hell#templating-yaml-is-a-terrible-terrible-idea
[yaml-hell]: /2023/01/11/the-yaml-document-from-hell

So that day,
when I was in a particularly defiant mood,
I decided to write my own configuration language.
With list comprehensions.
And types.

<img
  alt="I’ll build my own configuration language. With list comprehensions. And types."
  src="/images/ill-build-my-own-configuration-language.png"
  style="max-width: 18em; display: block; margin-left: auto; margin-right: auto;" />

I never expected or intended for it to go anywhere
— it was just a way to vent.
But six months later,
_Ruud’s Configuration Language_ is no longer complete vaporware.
I find it increasingly useful,
and I think it might benefit others too.
So let’s dive in!

## A functional foundation

To be clear, I’m not criticizing the designers of Ansible or HCL.
The limits of these tools are a natural consequence of their organic growth:
you start out with a tool that needs simple configuration,
adoption grows and people start doing more complex things with it,
and suddenly you find yourself without a good way to do abstraction.
So as a quick stopgap,
you [bolt on][hcl-loop] control flow
[encoded inside][ansible-loop] the [data format][gha-loop],
because that’s easy to do within the limits of the existing syntax.

When it comes to adding more principled abstraction features,
the authors have a background in infrastructure administration,
not in language design or type theory.
So they accidentally implement [some functions][hcl-flatten]
in an ad-hoc way that seemed helpful,
but causes surprises down the line.
(A `flatten` that _sometimes_ flattens recursively can’t be typed properly,
which breaks generic code.)
Many of Javascript and PHP’s idiosyncrasies can be explained in the same way.

The [Nix language][nix] had a more solid foundation
in functional programming from the start,
which enables abstraction in a natural way.
Even though it predates Terraform by more than a decade,
the language has stood the test of time far better than HCL.
With very few changes,
it scaled to massive configuration repositories like [Nixpkgs][nixpkgs],
and although Nix has issues,
abstracting repetition away is not one of them.
I’ve used Nix to generate repetitive GitHub Actions workflows,
and of course it is at the heart of NixOS,
where it generates configuration files such as systemd units
from a consistent declarative specification.
This is the power of having few simple features that compose well.

Though Nix is great,
I don’t think it is the answer to all configuration problems.
Nix-the-language is intimately tied to Nix-the-package-manager and the Nix store,
and the Haskell-style syntax can look foreign
to people who are used to more mainstream languages.
Still,
Nix has many good ideas that have been proven to work,
and my own configuration language is heavily inspired by it.

The other language that I take a lot of inspiration from is Python.
Python is not primarily a functional language,
but you can certainly use it in that way
(avoid mutation, write pure functions, prefer list comprehensions over loops, etc.),
and this is very natural.
I find the syntax pleasant and readable:
the meaning of idiomatic Python code is clear
even to people who are not intimately familiar with the language.
As a configuration language,
Python is not bad!
In fact,
I’ve _also_ used Python to generate repetitive GitHub Actions configurations.
List and dict literals are very similar to json,
and with functions, list comprehensions, and format strings,
there is ample room to abstract repetitive configuration.
Types can help to document and enforce structure.

But like Nix,
I don’t think that Python is the answer to all configuration problems.
A Python file is still primarily code,
not data,
and it’s usually not easy to inspect or evaluate pieces in isolation.
It is possible to make the entry point `json.dump` data to stdout,
but a Python program is still fundamentally a _program_ that has side-effects.

For my own language,
I took the parts that I like about Nix
(functional,
more data than code,
but with enough room to code when needed,
simple features that compose well),
Python
(clean and familiar syntax, list comprehensions, format strings, types),
and — consciously or unconsciously —
many more languages that I’ve been exposed to,
and combined them into a language
that _I_ like to work with.

[hcl-loop]:     https://developer.hashicorp.com/terraform/language/v1.7.x/meta-arguments/for_each
[ansible-loop]: https://docs.ansible.com/ansible/latest/playbook_guide/playbooks_loops.html#standard-loops
[gha-loop]:     https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idstrategymatrix
[hcl-flatten]:  https://developer.hashicorp.com/terraform/language/v1.7.x/functions/flatten
[nix]:          https://nixos.org/manual/nix/stable/language/index.html
[nixpkgs]:      https://github.com/NixOS/nixpkgs
[nix-tojson]:   https://nixos.org/manual/nix/stable/language/builtins#builtins-toJSON

## Oh no, yet another configuration language!

I am not the first person to be frustrated
by the lack of abstraction features in various tools,
nor am I the first person to think
that a configuration language would solve that.
There exist more configuration languages than I can count on one hand already
(see [the appendix][appendix]),
and probably many more that I’m not aware of.
So why add one more to the mix?
Why is _this one_ going to _really_ solve all our problems,
when five others haven’t seen widespread adoption (yet)?

First of all,
I did not start out writing my own language thinking
it would be a viable alternative to existing configuration languages.
I started it to vent,
because I find it fun to work on,
because it’s a good learning exercise,
and because I can do things in exactly the way that _I_ want to.
Dhall has been around longer,
has wider support,
and a bigger community.
But I don’t really like the syntax and the way it names some things.
That’s a superficial complaint,
and if I was looking for a tool to solve
my configuration problems with the least amount of effort,
then I can set my taste aside — I’ll get used to it.
But for a personal project that I spend my free time on,
I enjoy exploring ideas that form exactly the tool that _I_ want to have.

So that’s how it started,
as a toy project.
I put a big vaporware warning on it,
expecting that I would lose interest in it
before it got to a point where it was useful.
It’s certainly [not the first time][pris]
that I’m writing a toy language that stalled,
and maybe this one will meet the same fate.
(I do still occasionally use Pris,
and occasionally I get excited about adding features,
but it’s mostly abandoned,
like many of my side projects.)
But then my tool started being useful.
First in unexpected places
(as a `jq` replacement, more on that below),
and as I added features,
in more places,
to the point where now
— despite its shortcomings —
I would prefer it over some of the tools that I use at my day job.

So now what, is it a Serious Software Project now?
No, it’s still a hobby project without stability promise.
I don’t recommend using it for anything serious.
But it’s also _useful_ to the point
where I expect I’ll keep it in my toolbelt for the forseeable future
— if only as a `jq` replacement.
And if it’s useful to me,
maybe it’s useful to others,
so that’s why I’m writing about it today.

[appendix]: #appendix-other-configuration-languages
[pris]:     /2017/04/27/a-language-for-designing-slides

## Ruud’s Configuration Language

So what is this language?
I call it RCL,
jokingly named after myself,
but it turns out that `rcl` is a pretty good file extension
and name for a command-line tool.
If you prefer,
it might stand for Reasonable Configuration Language,
or, in classic GNU style,
for RCL configuration language.

The language is a superset of json.
This makes it easy to export data
from many tools and incrementally upgrade it to RCL,
including from yaml: just serialize it to json,
and you’re good to go.
This is a valid RCL document:

```
{
  "buckets": [
    {
      "name": "bucket-0",
      "location": "eu-west1",
      "delete-after-seconds": 86400
    },
    {
      "name": "bucket-1",
      "location": "eu-west1",
      "delete-after-seconds": 86400
    }
  ]
}
```

It’s 2024, so RCL has some features
that you might expect from a “modern” language:
trailing commas and numeric underscores.
Furthermore, dicts can be written with `ident = value` syntax
to omit the quotes and reduce some line noise:

```
{
  buckets = [
    {
      name = "bucket-0",
      location = "eu-west1",
      delete-after-seconds = 86_400,
    },
    {
      name = "bucket-1",
      location = "eu-west1",
      delete-after-seconds = 86_400,
    },
  ],
}
```

There are arithmetic expressions as you would expect, list comprehensions,
format strings, and functions:

```
{
  buckets = [
    for i in std.range(0, 2):
    {
      name = f"bucket-{i}",
      location = "eu-west1",
      delete-after-seconds = 24 * 3600,
    },
  ],
}
```

This is just a quick overview of some features.
For a more thorough introduction,
check out [the tutorial][rcl-tutorial]
and [the syntax guide][rcl-syntax].

An RCL document is always an expression,
and you can evaluate it to a json document with the `rcl` command-line tool:

```
rcl evaluate --output=json buckets.rcl
```

The tool can also output in RCL syntax,
which is a bit less noisy when inspecting data,
and it’s a way to upgrade json documents to RCL.
Aside from the standalone command-line tool,
I also recently added a Python module
that enables importing RCL documents in much the same way as `json.loads`.

Abstraction in a single document is nice,
but the real power comes from _imports_.
These allow you to break down configuration into small reusable pieces.
Let’s say that all your cloud resources are in the same location.
Then we might have a file `cloud_config.rcl`:

```
{
  location = "eu-west1",
}
```

Then in `buckets.rcl`, we can use that like so:

```
let cloud_config = import "cloud_config.rcl";
{
  buckets = [
    for i in std.range(0, 2):
    {
      name = f"bucket-{i}",
      location = cloud_config.location,
      delete-after-seconds = 24 * 3600,
    },
  ],
}
```

Because every document is an expression,
you can always evaluate it and inspect it,
even if it’s only an intermediate stage in a larger configuration.
For more fine-grained inspection,
there is [`trace`][rcl-trace],
and with `rcl query` you can evaluate an expression
against a document to drill down into it.
For example, to look only at the first bucket:

```
rcl query buckets.rcl 'input.buckets[0]'
```

This feature is what made RCL useful
for a use case that I did not anticipate:
querying json documents.

[rcl-syntax]:   https://docs.ruuda.nl/rcl/syntax/
[rcl-trace]:    https://docs.ruuda.nl/rcl/syntax/#debug-tracing
[rcl-tutorial]: https://docs.ruuda.nl/rcl/tutorial/

## An unexpected jq replacement

I use [jq][jq] a lot.
Most of the time,
only to pretty-print a json document returned from some API.
Because RCL is a superset of json, `rcl` can do that too now:

    curl --silent https://api.example.com | rcl evaluate

By itself that is nothing special,
the true power comes when querying.
Jq features its own stream processing DSL,
and for simple expressions I can usually remember the syntax
— unpack the list, extract a few fields.
But when it gets more complex,
I’m at a loss.
A while while ago,
I was dealing with a json document that had roughly this structure:

```json
[
  { "name": "widget-1", "tags": ["expensive", "fancy"] },
  { "name": "widget-2", "tags": ["cheap"] },
  { "name": "widget-3" },
  { "name": "widget-4", "tags": ["fancy", "intricate"] }
]
```

I wanted to know the names of all the widgets that had the `fancy` tag applied.
I spent about 10 minutes struggling with `jq` and scrolling through
unhelpful Stack Overflow answers.
I did not think to try ChatGPT at the time,
but in hindsight it _almost_ gets the query right
to a point where I could then get it working myself.
But fundamentally,
these kind of queries come up so infrequently,
that the things I learn about jq never really stick.
At that point I remembered:
I have a language in which this query is straightforward to express,
and it can import json!

```
$ rcl query --output=raw widgets.json '[
  for w in input:
  if w.get("tags", []).contains("fancy"):
  w.name
]'
widget-1
widget-4
```

That’s how RCL,
even though it is intended as a configuration language,
became one of my most frequently used query languages.

[jq]: https://jqlang.github.io/jq/

## Conclusion

To do: write a conclusion.

## Appendix: A non-exhaustive list of configuration languages

Aside from Nix, Python, and HCL, which I’ve already discussed extensively,
I am aware of the following configuation languages.
For the ones that I’ve used or at least evaluated briefly,
I’ve added my personal impression,
but note that these are very superficial.

 * **Cue** — TODO
 * **Dhall** — TODO
 * [**Jsonnet**](https://jsonnet.org/)
   — I never properly evaluated Jsonnet, but probably I should.
   Superficially it looks like one of the more mature formats,
   and in many ways it looks similar to RCL.
 * **KCL** — TODO
 * **Nickel** — TODO
 * **Starlark** — TODO
