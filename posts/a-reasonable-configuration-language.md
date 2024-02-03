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
The particular issue I was struggling with,
was to define six cloud storage buckets in Terraform.
They were similar, but not quite identical.
The kind of thing you’d do with [a two-line nested loop][rcl-loop]
in any language that has them,
but where all the ways of achieving that in HCL were so much hassle,
that is was far simpler to just copy-paste the config six times.

Although this HCL episode was the droplet,
my bucket of frustration had been filling up
for a longer time with challenges like these:

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

## The problem

Starts out as a simple data format. Then repetition. So you bolt it on.
Terraform for_each;
Ansible with;
If not possible inside, you do it outside, templating yaml (gasp).

But this is a solved problem! Nix!

## I’ll build my own configuration language!

With list comprehensions. And types.

## Status

Something something, toy project, lose interest. But actually, already useful.
Not going away entirely.

## An unexpected jq replacement

## Why another configuration language?

There are already so many languages that claim to solve the
“write repetitive configuration in a nice way”-problem.
Why add another one to the pile?

To do: add list of alternatives.

## Conclusion

To do: write a conclusion.
