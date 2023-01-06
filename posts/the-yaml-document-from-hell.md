---
title: The yaml document from hell
date: 2023-01-02
minutes: ?
synopsis: As a data format, YAML is an extremely complicated and has many footguns. In this post I explain some of those pitfalls by means of an example.
run-in: For a data format
---

For a data format, yaml is extremely complicated.
It aims to be a more human-friendly alternative to json,
but in striving for that it introduces so much complexity,
that I would argue it achieves the opposite result.
Yaml is full of footguns and its friendliness is deceptive.
In this post I want to demonstrate this through an example.

Yaml is really, really complex
------------------------------

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
This means that it is generally very difficult for a human to predict
how a particular document will parse.
This can lead to surprises.
Let’s look an example to highlight some of those.

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

The yaml document from hell
---------------------------

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

Let’s start with something that you might find in any container runtime configuration:

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
It depends on the language how that maps to json — if at all.
In Python it becomes the string `"True"`.
I would be really curious to know whether [GitHub Actions’ parser][gha-on]
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
If you think the list is a contrived example,
imagine updating a config file that lists a single value of 9.6.24
and changing it to 10.23.
Would you remember to add the quotes?
What makes this even more insidious
is that many dynamically typed applications
implicitly convert the number to a string when needed,
so your document works fine most of the time,
except in some contexts is doesn’t.
For example,
the following Jinja template accepts both
`version: "0.0"` and `version: 0.0`,
but it only emits output for the former.

```
{% if version %}Latest version: {{ version }}{% endif %}
```

## Runners-up
There is only so much I can fit into one artifical example.
Some arcane behaviors that did not make it in
are [directives][directives],
integers starting with `0` being octal literals (but only in yaml 1.1),
`~` being an alternative spelling of `null`,
and `?` introducing a [complex mapping key][complex-mapping-key].

[directives]: https://yaml.org/spec/1.2.2/#68-directives
[complex-mapping-key]: https://yaml.org/spec/1.2.2/#example-mapping-between-sequences

Syntax highlighting will not save you
-------------------------------------
You may have noticed that none of my examples have syntax highlighting enabled.
Maybe I am being unfair to yaml,
because syntax highlighting would highlight special constructs,
so you can at least see that some values are not normal strings.
However, due to multiple yaml versions being prevalent,
and highlighters having various levels of sophistication,
you can’t rely on this.
Vim, my blog generator, GitHub, and Codeberg,
all have a unique way to highlight the example document from this post.
No two of them pick out the same subset of values as non-strings!

The YAML spec
-------------
From [section 1.1 of the YAML spec][spec1.1],
its goals are, in order of decreasing priority:

 1. To be easily readable by humans.
 7. To be easy to implement and use.

In other words, YAML is explicitly not designed to be easy to use.

[spec1.1]: https://yaml.org/spec/1.2.2/#11-goals


Templating YAML
---------------
Don’t. It’s impossible. Generate JSON instead.

Alternatives
------------

 * Json.
 * TOML.
 * Hujson.
 * Nix.
 * Honorable mention: Cue, Dhall, maybe HCL.

Conclusion
----------

TODO.
