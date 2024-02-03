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
Ruud’s Configuration Language is no longer complete vaporware.
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
It predates Terraform by more than a decade,
and the language has stood the test of time far better than HCL did.
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
and my own configuration language takes lots of inspiration from it.

[hcl-loop]:     https://developer.hashicorp.com/terraform/language/v1.7.x/meta-arguments/for_each
[ansible-loop]: https://docs.ansible.com/ansible/latest/playbook_guide/playbooks_loops.html#standard-loops
[gha-loop]:     https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#jobsjob_idstrategymatrix
[hcl-flatten]:  https://developer.hashicorp.com/terraform/language/v1.7.x/functions/flatten
[nix]:          https://nixos.org/manual/nix/stable/language/index.html
[nixpkgs]:      https://github.com/NixOS/nixpkgs
[nix-tojson]:   https://nixos.org/manual/nix/stable/language/builtins#builtins-toJSON

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
