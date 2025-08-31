---
title: Automating configuration updates with rcl patch
date: 2025-08-31
lang: en-US
synopsis: TODO
minutes: 999
run-in: Sometimes we need automation
teaser: a-float-walks-into-a-gradual-type-system
---

Sometimes we need automation to update configuration files.
But formats that humans can read and maintain,
are hard for scripts to edit safely.
How do you automatically bump a version number without losing comments,
breaking formatting,
or replacing the wrong value?

## Automating configuration edits

Editing a json file from a script is easy enough:
deserialize it,
modify the data structure,
and serialize it again.
But json is more data than configuration,
and it doesn’t have comments.
As soon as we add comments though,
regular deserialization/serialization no longer works.
How can we reconcile keeping files maintainable for humans,
with enabling automation to update them?
We have a few options:

**Split out automation-managed parts.**
Keep the main configuration in a human-friendly format,
and put automation-managed values in separate files.
The automation-managed files don’t need comments,
and we don’t care about preserving formatting,
so they are easy to rewrite.
While some tools natively support splitting configuration across files,
for others we need a way to merge the separate files
back into a single configuration.
Templating configuration files is rarely a good idea,
but _generating_ them from a more expressive format is an option.
Below we’ll see how RCL enables
the human-managed vs. automation-managed split using imports.

**Hack it with text substitution.**
A little `sed` goes a long way:
regex-match on the old value and replace it with the new value.
Because we operate on text,
this in principle works for any format,
though it’s generally unsafe.
As with templating configuration files,
it suffers from injection and escaping problems.
Furthermore,
reliably locating the value to substitute can be tricky.
Consider the following file:

```toml
[kubernetes]
# Be sure to verify that our fleet is on
# a compatible kernel before updating!
version = "1.29.0"

[nginx]
version = "1.29.0"
```

How can we bump Nginx from `1.29.0` to `1.29.1` without touching Kubernetes?
Building a general patching tool based on string matching is hard,
but usually we don’t need to handle _any_ file,
only the configurations that we use in practice,
which are under our control.
If locating the right `1.29.0` is too hard,
we can add an <code>#&nbsp;auto-update: nginx</code> comment
to the version line to have something to match on.
So while text substitution is a hack,
it is a very practical one.

**Syntax-aware editing.**
Where text substitution is a hack,
the proper way to update files is to parse them into a syntax tree,
and perform the edit there.
In order to preserve comments and formatting,
that tree needs to be a _concrete syntax tree_.
There are not many tools that can do this,
but since v0.10.0,
RCL supports this natively through `rcl patch`.
We’ll see how below.

**Throwing an LLM at it.**
In 2025, there is a fourth option:
prompting an LLM to edit the configuration file.
This exacerbates the safety and reliability problems of text substitution,
introduces even more complexity than syntax-aware editing,
and uses thousands of times more compute resources.
I expect it to work well enough in practice,
but I consider this bad engineering,
and not a serious solution.

## Automation-friendly configuration with RCL

[R<!---->C<!---->L](https://rcl-lang.org/) is a new configuration language
that I’m building.
It extends json into a simple functional language
that enables abstraction and reuse.
It’s a more principled approach than templating configuration files,
and it enables modularity for tools that don’t natively support it.

We can express the example from before as follows in RCL:

```
{
  kubernetes = {
    // Be sure to verify that our fleet is on
    // a compatible kernel before updating!
    version = "1.29.0",
  },
  nginx = { version = "1.29.0" },
}
```

Running this through `rcl evaluate --format=toml` will produce
the same toml file as before
(though without the comment).
With [imports], we can now split out the automation-managed parts:

```
// kubernetes_version.json:
"1.29.0"

// nginx_version.json:
"1.29.0"

// config.rcl:
{
  kubernetes = { version = import "kubernetes_version.json" },
  nginx = { version = import "nginx_version.json" },
}
```

This evaluates to the same toml file as before,
but now a script can easily rewrite the version files.
This is powerful,
but the downsides are apparent even in this simple example:
the main configuration becomes more difficult to read,
there is a sprawl of small files,
and it’s hard to see at a glance what versions we are running.
The additional indirection makes `grep` less directly useful too.
Finally, we have no good place to put the warning comment any more.
If we put it in the version file,
that makes the file more difficult to update,
but if we put it in the main configuration,
it would not show up in a version bump diff to remind the reviewer!

To keep the configuration simple
while still enabling automation to edit it,
RCL now features [`rcl patch`][patch],
a built-in way to do syntax-aware editing.
We can use it to bump the Nginx version in the original file like so:

    rcl patch --in-place config.rcl nginx.version '"1.29.1"'

This update is safe:
because `rcl patch` operates on syntax trees,
the result is guaranteed to parse correctly,
and the edit can’t affect the tree except in the intended location.
Because the edit acts on the concrete syntax tree,
it preserves comments and core formatting.
Because RCL is a functional language,
we can be sure that replacing an expression
does not cause unintended side effects elsewhere in the document.
Finally,
we can pinpoint precisely which value to replace.

As of the recently released v0.10.0,
`rcl patch` is available as part of the command-line program.
For deeper integration into scripts,
RCL also features a Python module.
I plan to expose the patch functionality there in a future version.

[imports]: https://docs.ruuda.nl/rcl/imports/
[patch]:   https://docs.ruuda.nl/rcl/rcl_patch/

## Conclusion

Software needs configuration.
Software is not static, and configurations change over time.
Automation can help
to make routine configuration edits such as version bumps
less tedious and error-prone.
Configuration however,
is usually designed to be pleasant for humans to read and write,
not to be easy for automation to modify.
We can solve this by splitting the automation
into a machine-managed and a human-managed part,
but that makes the configuration more complex.
Alternatively,
we can move the complexity into the automation.
The generic and pragmatic way to update config files
is using text substitution with tools like `sed`,
but this approach has caveats.
The proper way is to use syntax-aware edits,
but tooling support for that is more limited.

The RCL configuration language supports
multiple approaches that enable automation
to update configuration.
With imports,
it can combine human-managed and machine-managed files
into a single result,
and with `rcl patch`,
it has a built-in way to safely edit RCL documents
while preserving comments and formatting.
