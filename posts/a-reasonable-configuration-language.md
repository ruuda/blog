---
title: A reasonable configuration language
date: 2024-02-04
lang: en-US
minutes: 17
synopsis: I was fed up with the poor opportunities for abstraction in configuration formats. The many configuration languages that exist already were not invented here, so I wrote my own, at first just for fun. But then it became useful.
teaser: a-type-system-for-rcl-part-1-introduction
run-in: About six months ago
---

About six months ago, I was fed up with it.
The particular _it_ was HCL — Hashicorp Configuration Language —
but that was just the trigger,
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
for a long time:

 * **GitHub Actions workflows** that differ in only a few commands
   — there is no native way to abstract those.
   The same applies to jobs within a workflow.
 * **Kubernetes manifests** that are 80% the same for most applications,
   and an entire industry that fails to adopt a proper solution for this,
   and instead resorts to templating yaml,
   which to me is [very obviously very wrong on so many levels][template-yaml].
 * **The prevalence of yaml in general**,
   a format that does solve some problems
   (adding comments and a lighter syntax to json),
   but in the process introduces [so many new problems][yaml-hell]
   that the cure is almost as bad as the disease.
 * **Ansible playbooks** that are too similar to justify duplicating,
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
[_Ruud’s Configuration Language_][rcl-github] is no longer completely vaporware.
I find it increasingly useful,
and I think it might benefit others too.
So let’s dive in!

[rcl-github]: https://github.com/ruuda/rcl

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
Many of JavaScript and PHP’s idiosyncrasies can be explained in the same way.

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
and the ML-style syntax can look foreign
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
A Python file is still primarily code, not data.
You can have an entry point `json.dump` data to files or stdout,
but it’s not always easy to import or evaluate intermediate pieces in isolation.
Python’s module system is great for larger codebases,
but less suitable for sharing pieces of data between many small scripts.

For my own language,
I took the parts that I like about Nix:
functional,
more data than code,
but with enough room to code when needed,
and simple features that compose well.
I took what I like about Python:
the clean and familiar syntax, list comprehensions, format strings, and types.
And consciously or unconsciously,
I’m influenced by many more languages that I’ve been exposed to.
Those ideas I combined into a language that _I_ like working with.

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
when five more mature ones haven’t seen widespread adoption (yet)?

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
I enjoy exploring ideas and building exactly the tool that _I_ want to have.

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

[appendix]: #appendix-a-non-exhaustive-list-of-configuration-languages
[pris]:     /2017/04/27/a-language-for-designing-slides

## Ruud’s Configuration Language

So what is this language?
I call it RCL,
named after myself in Bender meme style,
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

```json
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

<!--
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
-->
<pre><code class="sourceCode">{
  <span class="n">buckets</span> = [
    {</span>
      <span class="n">name</span> = <span class="st">"bucket-0"</span>,
      <span class="n">location</span> = <span class="st">"eu-west1"</span>,
      <span class="n">delete-after-seconds</span> = <span class="dv">86_400</span>,
    },
    {
      <span class="n">name</span> = <span class="st">"bucket-1"</span>,
      <span class="n">location</span> = <span class="st">"eu-west1"</span>,
      <span class="n">delete-after-seconds</span> = <span class="dv">86_400</span>,
    },
  ],
}</code></pre>

There are arithmetic expressions as you would expect, list comprehensions,
format strings, and functions:

<!--
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
-->
<pre><code class="sourceCode">{
  <span class="n">buckets</span> = [
    <span class="kw">for</span> <span class="n">i</span> <span class="kw">in</span> <span class="nb">std</span>.<span class="n">range</span>(<span class="dv">0</span>, <span class="dv">2</span>):
    {
      <span class="n">name</span> = <span class="st">f"bucket-</span><span class="dt">{</span><span class="n">i</span><span class="dt">}</span><span class="st">"</span>,
      <span class="n">location</span> = <span class="st">"eu-west1"</span>,
      <span class="n">delete-after-seconds</span> = <span class="dv">24</span> <span class="o">*</span> <span class="dv">3600</span>,
    },
  ],
}</code></pre>

For validation, the [`key_by`][key-by] method is useful.
In the above example,
if we’d name the buckets by hand and there are many of them,
how do we ensure that we don’t accidentally create two buckets with the same name?
We can do that by building a mapping from name to bucket:

<!--
let buckets = [
  // Omitted here for brevity, defined as before.
];

// Build a mapping of bucket name to bucket. If a key (bucket name)
// occurs multiple times, this will fail with an error that reports
// the offending key and the associated values. The type annotation
// is for clarification, it is not mandatory.
let buckets_by_name: Dict[String, Dynamic] = buckets.key_by(b => b.name);

// Constructing the mapping is enough for validation, the document still
// evaluates to the same dict as before. Note, the left "buckets" is the
// name of the field, the right "buckets" is a variable reference.
{ buckets = buckets }
-->
<pre><code class="sourceCode"><span class="kw">let</span> <span class="n">buckets</span> = [
  <span class="co">// Omitted here for brevity, defined as before.</span>
];

<span class="co">// Build a mapping of bucket name to bucket. If a key (bucket name)</span>
<span class="co">// occurs multiple times, this will fail with an error that reports</span>
<span class="co">// the offending key and the associated values. The type annotation</span>
<span class="co">// is for clarification, it is not mandatory.</span>
<span class="kw">let</span> <span class="n">buckets_by_name</span>: <span class="dt">Dict</span>[<span class="dt">String</span>, <span class="dt">Dynamic</span>] = <span class="n">buckets</span>.<span class="fu">key_by</span>(<span class="n">b</span> <span class="o">=&gt;</span> <span class="n">b</span>.<span class="n">name</span>);

<span class="co">// Constructing the mapping is enough for validation, the document still</span>
<span class="co">// evaluates to the same dict as before. Note, the left "buckets" is the</span>
<span class="co">// name of the field, the right "buckets" is a variable reference.</span>
{ <span class="n">buckets</span> = <span class="n">buckets</span> }
</code></pre>

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

<pre><code class="sourceCode">{
  default_location = <span class="st">"eu-west1"</span>,
}</code></pre>

Then in `buckets.rcl`, we can use that like so:

<!--
let cloud_config = import "cloud_config.rcl";
{
  buckets = [
    for i in std.range(0, 2):
    {
      name = f"bucket-{i}",
      location = cloud_config.default_location,
      delete-after-seconds = 24 * 3600,
    },
  ],
}
-->
<pre><code class="sourceCode"><span class="kw">let</span> cloud_config = <span class="kw">import</span> <span class="st">"cloud_config.rcl"</span>;
{
  <span class="n">buckets</span> = [
    <span class="kw">for</span> <span class="n">i</span> <span class="kw">in</span> <span class="nb">std</span>.<span class="n">range</span>(<span class="dv">0</span>, <span class="dv">2</span>):
    {
      <span class="n">name</span> = <span class="st">f"bucket-</span><span class="dt">{</span><span class="n">i</span><span class="dt">}</span><span class="st">"</span>,
      <span class="n">location</span> = cloud_config.default_location,
      <span class="n">delete-after-seconds</span> = <span class="dv">24</span> <span class="o">*</span> <span class="dv">3600</span>,
    },
  ],
}</code></pre>

Because every document is an expression,
you can always evaluate it and inspect it,
even if it’s only an intermediate stage in a larger configuration.
For more fine-grained inspection there is [`trace`][rcl-trace],
and with [`rcl query`][rcl-query] you can evaluate an expression
against a document to drill down into it.
For example, to look only at the first bucket:

```
rcl query buckets.rcl 'input.buckets[0]'
```

This feature is what made RCL useful
for a use case that I did not anticipate:
querying json documents.

[key-by]:       https://docs.ruuda.nl/rcl/type_list/#key_by
[rcl-query]:    https://docs.ruuda.nl/rcl/rcl_query/
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
  { "name": "server-1", "tags": ["amd", "fast"] },
  { "name": "server-2", "tags": ["intel", "slow"] },
  { "name": "server-3" },
  { "name": "server-4", "tags": ["amd", "vm", "slow"] }
]
```

I wanted to know the names of all the machines that had a particular tag applied.
That the `tags` field is missing from some machines complicates that,
and the real input consisted of hundreds of machines,
so fixing that by hand was not feasible.
I spent about 10 minutes struggling with `jq` and scrolling through
unhelpful Stack Overflow answers.
I did not think to try ChatGPT at the time,
but in hindsight it _almost_ gets the query right
to a point where I could then get it working myself.
But fundamentally,
these kind of queries come up so infrequently
that the things I learn about `jq` never really stick.
ChatGPT is no excuse to tolerate bad tools:
if the one-liner is easy to write,
that’s still faster than leaving your terminal.
At that point I remembered:
I have a language in which this query is straightforward to express,
and it can import json!

```
$ rcl query --output=raw machines.json '[
  for m in input:
  if m.get("tags", []).contains("amd"):
  m.name
]'
server-1
server-4
```

That’s how RCL,
even though it is intended as a configuration language,
became one of my most frequently used query languages.

[jq]: https://jqlang.github.io/jq/

## The future of RCL

That day when I was fed up with HCL and I ran `git init`,
I didn’t expect to produce anything useful
aside from entertaining myself for a few evenings.
Now six months later,
RCL is no longer vaporware,
and it regularly solves real problems for me!

Some parts of RCL are already quite polished.
It has mostly good error reporting,
[there is reference documentation][rcl-docs],
it has an autoformatter,
and it is very well tested with a suite of golden tests and fuzzers.
Although I’m not sure at what point
it starts being worth the complication of an additional tool,
RCL can define cloud storage buckets today
with [Terraform’s json syntax][tf-json].
But RCL is also far from ready for prime time:
there is no syntax highlighting for any editor aside from Vim,
the type system is a work in progress,
it doesn’t support floats yet,
the Python module doesn’t expose errors nicely,
the autoformatter has quirks,
and I’m still ambivalent about whether there should be a `:` after `else`.

But most of all,
I’m not sure whether I _want_ RCL to experience prime time.
Of course it is very gratifying to see your project be adopted
and solve real-world problems for other people.
I’m proud of what I built so far and I _want_ people to see it and try it
— that’s why I publish everything as free and open source software,
and that’s why I’m writing this post.
It always cheers me up
when somebody who found one of my projects useful or interesting sends me an e-mail.
But I also already experience a bit of maintainer fatigue
from some of my successful Rust crates,
and I don’t always spend the time on them that they deserve.
When a project takes off,
inevitably users start making requests,
having opinions,
and submitting well-intentioned but low-quality contributions.
Keeping up with that takes time and mental energy.
I like working on RCL right now,
because I get to build it in exactly the way I want,
and it solves exactly the problems that I have.
Building a tool for the open source community
would require making different trade-offs.
For now, I’m treating it as a source-available project.
It solves a need for me,
and if others find it useful that’s great,
but it is provided as-is.
Maybe Haskell’s _avoid success at all cost_ isn’t such a bad idea.

[rcl-docs]: https://docs.ruuda.nl/rcl/
[tf-json]:  https://developer.hashicorp.com/terraform/language/syntax/json

## Appendix: A non-exhaustive list of configuration languages

Aside from Nix, Python, and HCL, which I’ve already discussed extensively,
I am aware of the following configuation languages.
For the ones that I’ve used or at least evaluated briefly,
I added my personal impressions,
but beware that these are very superficial.

[**Bicep**](https://github.com/Azure/bicep)
— Microsoft’s DSL for configuring Azure resources declaratively.
I haven’t looked into it in much detail because I don’t work with Azure,
but it looks potentially interesting.

[**Cue**](https://cuelang.org/)
— Out of all the configuration languages
that I evaluated during a company hackathon,
I found Cue to be the most promising one.
Its type system is interesting:
it helps to constrain and validate configuration
(as you would expect from a type system),
but it also plays a role in eliminating boilerplate.
Like Nix,
Cue is based on few simple constructs that compose well,
and grounded in solid theory.
It took me some time before it clicked,
but when it did,
Cue became really powerful.
A few things I don’t like about it
are the package/module system that has its roots in the Go ecosystem,
and its string interpolation syntax which is hideous.
The command-line tooling works but could be more polished,
and I found it to become slow quickly,
even for fairly small configurations.
It has [a page][cue-compr]
comparing itself against a few other configuation languages.

[**Dhall**](https://dhall-lang.org/)
— This is the first configuration language that I learned about many years ago.
From what I can tell,
it is one of the most mature and widely supported configuration languages.
I use [Spago][spago],
the PureScript package manager,
in some of my projects,
and it uses Dhall as its configuration format.
Unfortunately it looks like it is being [deprecated][spago-depr] in favor of yaml.
I tried to use Dhall once to solve an Advent of Code challenge,
but got stuck immediately because it’s not possible to split strings in Dhall.
Of course,
this is an unfair test to evaluate a configuration language on,
but it does give an impression of the expressivity of a language.
I’ve used Nix to [solve][aoc-nix] a few Advent of Code challenges in the past,
and this year I [solved][aoc-rcl] a few in RCL,
which went pretty well for small inputs,
but the lack of unbounded loops and tail calls
make it unsuitable as a general-purpose language.
Although I used to work as a Haskell developer,
the formatting and names of built-in functions in Dhall look awkward to me.

[**JSON-e**](https://json-e.js.org/)
— A json parametrization language.
I discovered this one in Rimu’s list of related projects.
I think I’ve seen it mentioned a few times before,
but I haven’t evaluated it at all.

[**Jsonnet**](https://jsonnet.org/)
— I never properly evaluated Jsonnet, but probably I should.
Superficially it looks like one of the more mature formats,
and in many ways it looks similar to RCL.
Its has [a page][jsonnet-compr]
comparing itself against other configuration languages.

[**KCL**](https://kcl-lang.io/)
— This is an odd one.
From the website and repository it looks like
a lot of resources went into this project,
but somehow I’ve never seen it come up or be used anywhere.
I only learned about it when I started searching for configuration languages.
From the way it describes itself,
it sounds like the tool I want,
but I am generally wary of tools that use lots of buzzwords,
especially when it involves the words “modern” and “cloud native”.
I should evaluate it properly at some point.
It has [a page][kcl-compr]
comparing itself against other configuation languages.

[**Nickel**](https://nickel-lang.org/)
— [An attempt to create a language similar to Nix][nickel-intro],
but without being tied to the package manager and Nix store.
It looked very promising to me,
but after evaluating it during a company hackathon,
I found it difficult or impossible to express sanity checks
that I can easily express in Cue and RCL.
Its has [a page][nickel-compr]
comparing itself against other configuation languages.

[**Pkl**](https://pkl-lang.org/)
— A configuration language by Apple.
The timing is eerie:
I wrote this post on a Saturday
with the intention of proofreading and publishing it the next day,
and right that Sunday morning,
[the Pkl announcement post][pkl-announce] was on the Hacker News frontpage.
From the comments,
it [has been in use][pkl-used] at Apple internally for a few years already.
I haven’t had the opportunity to evaluate it yet.
Its has [a page][pkl-compr]
comparing itself against other configuation languages,
but only superficially.

[**Pulumi**](https://www.pulumi.com/)
— Not a configuration language,
but an infrastructure automation tool like Terraform.
It can be configured using existing general-purpose programming languages.
I haven’t had the opportunity to try it,
but I suppose I don’t get to complain about HCL
without at least acknowledging Pulumi’s existence.

[**Rimu**](https://rimu.dev/)
— I stumbled upon this one recently while browsing
[the configuration-language tag][gh-configlang] on GitHub.
It might be an eerie case of [parallel discovery][par-discovery]:
like RCL,
it looks like a configuration language developed as a side project,
written in Rust,
started in August 2023,
and not ready for serious use yet.
Unlike RCL, its syntax is based on yaml.

[**Starlark**](https://bazel.build/rules/language)
— A Python dialect used by the Bazel build tool.
I used it intensively when I was working with Blaze/Bazel,
and it works well for defining build targets.
Starlark has multiple implementations,
including [one in Rust][starlark-rust]
that can be used as a standalone command-line tool,
but all the implementations clearly focus on being embedded.
From my limited attempts
to use them in an infrastructure-as-code repository,
they are not suitable for incremental adoption there.

[**TypeScript**](https://www.typescriptlang.org/)
— Not a configuration language,
but it deserves a mention here,
because RCL intends to be json with abstraction and types,
and since TypeScript is a superset of JavaScript,
which is a superset of json,
it falls in the same category
of tools that can type and abstract json.
I haven’t used TypeScript enough to have a strong opinion on its type system.
Possibly RCL’s type system will end up being similar.

[aoc-nix]:       https://github.com/ruuda/adventofcode/blob/c452562c72cdd203df4dd0fd631596e6c0e2aa13/2022/03/main.nix
[aoc-rcl]:       https://github.com/ruuda/adventofcode/blob/c452562c72cdd203df4dd0fd631596e6c0e2aa13/2023/11/main.rcl
[cue-compr]:     https://cuelang.org/docs/usecases/configuration/#comparisons
[gh-configlang]: https://github.com/topics/configuration-language
[jsonnet-compr]: https://jsonnet.org/articles/comparisons.html
[kcl-compr]:     https://kcl-lang.io/docs/0.6.0/user_docs/getting-started/intro/#how-to-choose
[nickel-compr]:  https://github.com/tweag/nickel/blob/6cf2902d3db768618e1d990c549671e308dd3ff4/RATIONALE.md#comparison-with-alternatives
[nickel-intro]:  https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[par-discovery]: https://en.wikipedia.org/wiki/Multiple_discovery
[pkl-announce]:  https://pkl-lang.org/blog/introducing-pkl.html
[pkl-compr]:     https://pkl-lang.org/main/current/introduction/comparison.html
[pkl-used]:      https://news.ycombinator.com/item?id=39248081
[pulumi]:        https://www.pulumi.com/
[spago-depr]:    https://github.com/purescript/spago/tree/bbe37b6cd497aa544bd0761fa7a56a5f5d002a87#migrate-from-spagodhall-to-spagoyaml
[spago]:         https://github.com/purescript/spago
[starlark-rust]: https://github.com/facebookexperimental/starlark-rust
