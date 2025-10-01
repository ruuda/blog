---
title: Don’t confuse configuration formats for languages
date: 2025-10-01
lang: en-US
synopsis: Bikeshed about configuration formats is beside the point. Alternative formats solve superficial problems, languages solve the real ones.
minutes: ?
run-in: TODO
teaser: automating-configuration-updates
---

Recently a number of alternative configuration formats
have been making the rounds.
The world is growing tired of yaml
while toml is becoming more widespread,
in part due to tools like Cargo.
Json supersets
(with [comments][jcma],
[commas][jwcc],
and [the digit 5][json5])
are flourishing,
while kson and maml promise to hit the sweet spot
between friendly and simple.

[jcma]: https://code.visualstudio.com/docs/languages/json#_json-with-comments
[jwcc]: https://nigeltao.github.io/blog/2021/json-with-commas-comments.html
[Jsokn5][json5] is gaining traction recently,
[json5]: https://json5.org/

## Syntax is superficial


## Conclusion

Configuration format is not configuration language.
