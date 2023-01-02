---
title: The YAML document from hell
date: 2023-01-02
minutes: ?
synopsis: As a data format, YAML is an extremely complicated and has many footguns. In this post I explain some of those pitfalls by means of an example.
run-in: For a data format, YAML is extremely complicated.
---

For a data format, YAML is extremely complicated.
It aims to be a more human-friendly alternative to JSON,
but in striving for that it introduces so much complexity,
that I would argue that it achieves the opposite result.
It is full of footguns and its friendliness is deceptive.
In this post I want to explain this through an example.

<span class="run-in">Json</span> is simple.
[The entire JSON spec][json-spec] consists of six railroad diagrams.
It’s a simple data format with a simple syntax and that’s all there is to it.
<span class="run-in">Yaml</span> on the other hand, is complex.
So complex,
that [its specification][yaml-spec] consists of _10 chapters_
with sections numbered four levels deeps.

[json-spec]: https://www.json.org/json-en.html
[yaml-spec]: https://yaml.org/spec/1.2.2/

The YAML document from hell
---------------------------

Consider the following document.

```yaml
loadbalancer_config:
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
```

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
