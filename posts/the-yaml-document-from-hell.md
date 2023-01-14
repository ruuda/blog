---
title: The yaml document from hell
date: 2023-01-11
minutes: 14
synopsis: As a data format, yaml is extremely complicated and it has many footguns. In this post I explain some of those pitfalls by means of an example, and I suggest a few simpler and safer yaml alternatives.
run-in: For a data format
---

For a data format, yaml is extremely complicated.
It aims to be a human-friendly format,
but in striving for that it introduces so much complexity,
that I would argue it achieves the opposite result.
Yaml is full of footguns and its friendliness is deceptive.
In this post I want to demonstrate this through an example.

This post is a rant,
and more opinionated than my usual writing.

## Yaml is really, really complex

Json is simple.
[The entire json spec][json-spec] consists of six railroad diagrams.
It’s a simple data format with a simple syntax and that’s all there is to it.
Yaml on the other hand, is complex.
So complex,
that [its specification][yaml-spec] consists of _10 chapters_
with sections numbered four levels deep
and a dedicated [errata page][yaml-errata].

The json spec is not versioned.
There were [two changes][json-change] to it in 2005
(the removal of comments, and the addition of scientific notation for numbers),
but it has been frozen since
— almost two decades now.
The yaml spec on the other hand is versioned.
The latest revision is fairly recent, 1.2.2 from October 2021.
Yaml 1.2 differs substantially from 1.1:
the same document can parse differently under different yaml versions.
We will see multiple examples of this later.

Json is so obvious that
Douglas Crockford claims [to have discovered it][json-saga] — not invented.
I couldn’t find any reference for how long it took him to write up the spec,
but it was probably hours rather than weeks.
The change from yaml 1.2.1 to 1.2.2 on the other hand,
was [a multi-year effort by a team of experts][yaml-122-blog]:

> This revision is the result of years of work
> by the new YAML language development team.
> Each person on this team has a deep knowledge of the language
> and has written and maintains important open source YAML frameworks and tools.

Furthermore this team plans to actively evolve yaml, rather than to freeze it.

When you work with a format as complex as yaml,
it is difficult to be aware of all the features and subtle behaviors it has.
There is [an entire website][yaml-multiline]
dedicated to picking one of [the 63 different multi-line string syntaxes][yaml-63].
This means that it can be very difficult for a human to predict
how a particular document will parse.
Let’s look an example to highlight this.

[json-spec]:      https://www.json.org/json-en.html
[yaml-spec]:      https://yaml.org/spec/1.2.2/
[yaml-errata]:    https://yaml.org/spec/1.2/errata.html
[json-saga]:      https://www.youtube.com/watch?v=-C-JoyNuQJs
[json-change]:    https://youtu.be/-C-JoyNuQJs?t=965
[yaml-122-blog]:  https://yaml.com/blog/2021-10/new-yaml-spec/
[yaml-multiline]: https://yaml-multiline.info/
[yaml-63]:        https://stackoverflow.com/a/21699210/135889

<!--
This is the last version without scientific notation, 2005-07-21:
https://web.archive.org/web/20050721035358/http://www.crockford.com:80/JSON/index.html

This is the first version to feature scientific notation, 2005-07-24:
https://web.archive.org/web/20050724003319/http://www.crockford.com:80/JSON/index.html

This is the last version to feature comments, 2005-08-11:
https://web.archive.org/web/20050811233342/http://www.crockford.com:80/JSON/index.html

This is the first version to no longer feature comments, 2005-08-23:
https://web.archive.org/web/20050823002712/http://www.crockford.com:80/JSON/index.html
-->

## The yaml document from hell

Consider the following document.

```
server_config:
  port_mapping:
    # Expose only ssh and http to the public internet.
    - 22:22
    - 80:80
    - 443:443

  serve:
    - /robots.txt
    - /favicon.ico
    - *.html
    - *.png
    - !.git  # Do not expose our Git repository to the entire world.

  geoblock_regions:
    # The legal team has not approved distribution in the Nordics yet.
    - dk
    - fi
    - is
    - no
    - se

  flush_cache:
    on: [push, memory_pressure]
    priority: background

  allow_postgres_versions:
    - 9.5.25
    - 9.6.24
    - 10.23
    - 12.13
```

Let’s break this down section by section and see how the data maps to json.

## Sexagesimal numbers

Let’s start with something that you might find in a container runtime configuration:

```
port_mapping:
  - 22:22
  - 80:80
  - 443:443
```
```json
{"port_mapping": [1342, "80:80", "443:443"]}
```
Huh, what happened here?
As it turns out,
numbers from 0 to 59 separated by colons
are [sexagesimal (base 60) number literals][sexagesimal].
This arcane feature was present in yaml 1.1,
but silently removed from yaml 1.2,
so the list element will parse as `1342` or `"22:22"`
depending on which version your parser uses.
Although yaml 1.2 is more than 10 years old by now,
you would be mistaken to think that it is widely supported:
the latest version libyaml at the time of writing
(which is used among others by [PyYAML][pyyaml60])
implements yaml 1.1 and parses `22:22` as `1342`.

[sexagesimal]: https://yaml.org/spec/1.1/#id858600
[pyyaml60]: https://pypi.org/project/PyYAML/6.0/

## Anchors, aliases, and tags

The following snippet is actually invalid:

```
serve:
  - /robots.txt
  - /favicon.ico
  - *.html
  - *.png
  - !.git
```

Yaml allows you to create an _anchor_
by adding an `&` and a name in front of a value,
and then you can later reference that value with an _alias_:
a `*` followed by the name.
In this case no anchors are defined,
so the aliases are invalid.
Let’s avoid them for now and see what happens.

```
serve:
  - /robots.txt
  - /favicon.ico
  - !.git
```
```json
{"serve": ["/robots.txt", "/favicon.ico", ""]}
```
Now the interpretation depends on the parser you are using.
The element starting with `!` is a [tag][tag].
This feature is intended to enable a parser to convert
the fairly limited yaml data types
into richer types that might exist in the host language.
A tag starting with `!` is up to the parser to interpret,
often by calling a constructor with the given name
and providing it the value that follows after the tag.
This means that
**loading an untrusted yaml document is generally unsafe**,
as it may lead to arbitrary code execution.
(In Python,
you can avoid this pitfall by using `yaml.safe_load` instead of `yaml.load`.)
In our case above,
PyYAML fails to load the document because it doesn’t know the `.git` tag.
Go’s yaml package is less strict
and returns an empty string.

[tag]: https://yaml.org/spec/1.2.2/#3212-tags
[goyaml301]: https://github.com/go-yaml/yaml/tree/v3.0.1

## The Norway problem

This pitfall is so infamous
that it became known as “[the Norway problem][norway-problem]”:

```
geoblock_regions:
  - dk
  - fi
  - is
  - no
  - se
```
```json
{"geoblock_regions": ["dk", "fi", "is", false, "se"]}
```
What is that `false` doing there?
The literals `off`, `no`, and `n`,
in various capitalizations ([but not any capitalization][yaml-bool]!),
are all `false` in yaml 1.1,
while `on`, `yes`, and `y` are true.
In yaml 1.2 these alternative spellings of the boolean literals are no longer allowed,
but they are so pervasive in the wild
that a compliant parser would have a hard time reading many documents.
Go’s yaml library therefore [made the choice][go-yaml-compat]
of implementing a custom variant somewhere in between yaml 1.1 and 1.2
that behaves differently depending on the context:

> The yaml package supports most of YAML 1.2,
> but preserves some behavior from 1.1 for backwards compatibility.
> YAML 1.1 bools (yes/no, on/off) are supported
> as long as they are being decoded into a typed bool value.
> Otherwise they behave as a string.

Note that it only does that since version 3.0.0,
which was released in May 2022.
[Earlier versions behave differently][go-yaml-off].

[go-yaml-compat]: https://github.com/go-yaml/yaml/tree/v3.0.1#compatibility
[go-yaml-off]: https://github.com/go-yaml/yaml/commit/b145382a4cda47600eceb779844b8090b5807c4f
[norway-problem]: https://hitchdev.com/strictyaml/why/implicit-typing-removed/
[yaml-bool]: https://yaml.org/type/bool.html

## Non-string keys

While keys in json are always strings,
in yaml they can be any value,
including booleans.

```
flush_cache:
  on: [push, memory_pressure]
  priority: background
```
```json
{
  "flush_cache": {
    "True": ["push", "memory_pressure"],
    "priority": "background"
  }
}
```

Combined with the previous feature of interpreting `on` as a boolean,
this leads to a dictionary with `true` as one of the keys.
It depends on the language how that maps to json, if at all.
In Python it becomes the string `"True"`.
The key `on` is common in the wild
because [it is used in GitHub Actions][gha-on].
I would be really curious to know whether GitHub Actions’ parser
looks at `"on"` or `true` under the hood.

[gha-on]: https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#on

## Accidental numbers

Leaving strings unquoted can easily lead to unintentional numbers.

```
allow_postgres_versions:
  - 9.5.25
  - 9.6.24
  - 10.23
  - 12.13
```
```json
{"allow_postgres_versions": ["9.5.25", "9.6.24", 10.23, 12.13]}
```
Maybe the list is a contrived example,
but imagine updating a config file that lists a single value of 9.6.24
and changing it to 10.23.
Would you remember to add the quotes?
What makes this even more insidious
is that many dynamically typed applications
implicitly convert the number to a string when needed,
so your document works fine most of the time,
except in some contexts it doesn’t.
For example,
the following Jinja template accepts both
`version: "0.0"` and `version: 0.0`,
but it only takes the true-branch for the former.

```
{% if version %}
  Latest version: {{ version }}
{% else %}
  Version not specified
{% endif %}
```

## Runners-up

There is only so much I can fit into one artifical example.
Some arcane yaml behaviors that did not make it in
are [directives][directives],
integers starting with `0` being octal literals (but only in yaml 1.1),
`~` being an alternative spelling of `null`,
and `?` introducing a [complex mapping key][complex-mapping-key].

[directives]: https://yaml.org/spec/1.2.2/#68-directives
[complex-mapping-key]: https://yaml.org/spec/1.2.2/#example-mapping-between-sequences

## Syntax highlighting will not save you

You may have noticed that none of my examples have syntax highlighting enabled.
Maybe I am being unfair to yaml,
because syntax highlighting would highlight special constructs,
so you can at least see that some values are not normal strings.
However, due to multiple yaml versions being prevalent,
and highlighters having different levels of sophistication,
you can’t rely on this.
I’m not trying to nitpick here:
Vim, my blog generator, GitHub, and Codeberg,
all have a unique way to highlight the example document from this post.
No two of them pick out the same subset of values as non-strings!

## Templating yaml is a terrible, terrible idea

I hope it is clear by now that working with yaml is subtle at the very least.
What is even more subtle
is concatenating and escaping arbitrary text fragments in such a way
that the result is a valid yaml document,
let alone one that does what you expect.
Add to this the fact that whitespace is significant in yaml,
and the result is a format that is
[meme-worthily][memenetes] difficult to template correctly.
I truly do not understand
why [tools based on such an error-prone practice][helm]
have gained so much mindshare,
when there is a safer, easier, and more powerful alternative:
generating json.

[memenetes]: https://twitter.com/memenetes/status/1600898397279502336
[helm]:      https://helm.sh/docs/chart_best_practices/templates/

## Alternative configuration formats

I think the main reason that yaml is so prevalent despite its pitfalls,
is that for a long time it was the only viable configuration format.
Often we need lists and nested data,
which rules out flat formats like ini.
Xml is noisy and annoying to write by hand.
But most of all, we need comments,
which rules out json.
(As we saw before,
json had comments very early on,
but they were removed because people started putting parsing directives in there.
I think this is the right call for a serialization format,
but it makes json unsuitable as a configuration language.)
So if what we really need is the json data model
but a syntax that allows comments,
what are some of the options?

 * [**Tree**][tree] - put your opinion here, please
 * [**Toml**][toml] —
   Toml is similar to yaml in many ways:
   it has mostly the same data types;
   the syntax is not as verbose as json;
   and it allows comments.
   Unlike yaml it is not full of footguns,
   mostly because strings are always quoted,
   so you don’t have values that look like strings but aren’t.
   Toml is widely supported,
   you can probably find a toml parser for your favorite language.
   It’s even in the Python standard library — unlike yaml!
   A weak spot of toml is deeply nested data.
 * [**Json with comments**][jsonc],
   [**Json with commas and comments**][jwcc] —
   There exist various extensions of json that extend it just enough
   to make it a usable config format
   without introducing too much complexity.
   Json with comments is probably the most widespread,
   as it is used as the config format for Visual Studio Code.
   The main downside of these is that they haven’t really caught on (yet!),
   so they aren’t as widely supported as json or yaml.
 * **A simple subset of yaml** —
   Many of the problems with yaml are caused by unquoted things
   that look like strings
   but behave differently.
   This is easy to avoid: always quote all strings.
   (Indeed, you can tell that somebody is an experienced yaml engineer when
   they defensively quote all the strings.)
   We can choose to always use `true` and `false`
   rather than `yes` and `no`,
   and generally stay away from the arcane features.
   The challenge with this is that any construct not explicitly forbidden
   will eventually make it into your codebase,
   and I am not aware of any good tool that can enforce a sane yaml subset.

## Generating json as a better yaml

Often the choice of format is not ours to make,
and an application only accepts yaml.
Not all is lost though,
because yaml is a superset of json,
so any tool that can produce json can be used to generate a yaml document.

Sometimes an application will start out with a need for just a configuration format,
but over time you end up with many many similar stanzas,
and you would like to share parts between them,
and abstract some repetition away.
This tends to happen in for example Kubernetes and GitHub Actions.
When the configuration language does not support abstraction,
people often reach for templating,
which is a bad idea for the reasons explained earlier.
Proper programming languages,
possibly domain-specific ones,
are a better fit.
Some of my favorites are Nix and Python:

 * [**Nix**][nixlang] —
   Nix is the language used by the [Nix package manager][nix].
   It was created for writing package definitions,
   but it works remarkably well as a configuration format
   (and indeed it is used to configure NixOS).
   Functions, let-bindings, and string interpolation
   make it powerful for abstracting repetitive configuration.
   The syntax is light like toml,
   and it can [export to json][tojson] or xml.
   It works well for simplifying a repetitive GitHub Actions workflow file,
   for example.
 * [**Python**][python] —
   Json documents double as valid Python literals with minimal adaptation,
   and Python supports trailing commas and comments.
   It has variables and functions,
   powerful string interpolation,
   and [`json.dump`][pydump] built in.
   A self-contained Python file that prints json to stdout
   goes a long way!

Finally there are some tools in this category that I
haven’t used enough to confidently recommend,
but which deserve to be mentioned:

 * [**Dhall**][dhall] —
   Dhall is like Nix, but with types.
   It is less widespread,
   and personally I find the built-in function names unwieldy.
 * [**Cue**][cue] —
   Like Dhall, Cue integrates type/schema information into the config format.
   Cue is a superset of json,
   but despite that,
   I find the files that actually use Cue’s features to look foreign to me.
   Cue is on my radar to evaluate further,
   but I haven’t encountered a problem
   where Cue looked like the most suitable solution yet.
 * [**Hashicorp Configuration Language**][hcl] —
   I haven’t used HCL extensively enough to have a strong opinion on it,
   but in the places where I worked with it,
   the potential for abstraction seemed more limited
   than what you can achieve with e.g. Nix.

[cue]:     https://cuelang.org/
[dhall]:   https://dhall-lang.org/
[hcl]:     https://github.com/hashicorp/hcl
[jsonc]:   https://code.visualstudio.com/docs/languages/json#_json-with-comments
[jwcc]:    https://nigeltao.github.io/blog/2021/json-with-commas-comments.html
[nix]:     https://nixos.org/
[nixlang]: https://nixos.org/manual/nix/stable/language/index.html
[pydump]:  https://docs.python.org/3/library/json.html?highlight=json%20dump#json.dump
[python]:  https://www.python.org/
[tojson]:  https://nixos.org/manual/nix/stable/language/builtins.html#builtins-toJSON
[toml]:    https://toml.io/en/
[tree]:    https://dev.to/ninjin/tree-ast-which-kills-json-xml-yaml-toml-3kid

## Conclusion

Yaml aims to be a more human-friendly alternative to json,
but with all of its features,
it became such a complex format with so many bizarre and unexpected behaviors,
that it is difficult for humans to predict how a given yaml document will parse.
If you are looking for a configuration format,
toml is a friendly format without yaml’s footguns.
For cases where you are stuck with yaml,
generating json from a more suitable language
can be a viable approach.
Generating json also opens op the possibility for abstraction and reuse,
in a way that is difficult to achieve safely by templating yaml.
