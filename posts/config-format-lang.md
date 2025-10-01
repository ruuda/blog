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
the difference between these other formats is mostly superficial.
The one real difference is in their data models.
Most of them adopt the json data model of objects and arrays,
while KDL (like [HCL], and e.g. [Nginx][nginx] config) adopts
the XML data model of named nodes with attributes and children.
The rest is just syntax.
And yes, syntax _does_ matter,
but it doesn’t matter _that_ much.
Line noise is not the real problem here!

[nginx]:    https://nginx.org/en/docs/beginners_guide.html#conf_structure
[HCL]:      https://opentofu.org/docs/language/syntax/configuration/
[yamlhell]: /2023/01/11/the-yaml-document-from-hell

## Syntax is superficial

## How to apply this in your application

 * Deserialize struct.
 * Accept toml or json, and if you like, yaml or a json dialect.

## Conclusion

Configuration format is not configuration language.
