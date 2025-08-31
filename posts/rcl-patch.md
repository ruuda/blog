---
title: Automating configuration updates with rcl patch
date: 2025-08-31
lang: en-US
synopsis: TODO
minutes: 999
run-in: We want automation
teaser: a-float-walks-into-a-gradual-type-system
---

We want automation to update configuration files.
But formats that humans can read and maintain,
are hard for scripts to edit safely.
How do you automatically bump a version number without losing comments,
breaking formatting,
or replacing the wrong value?

## Scripting edits

Editing a json file from a script is easy:
deserialize, modify, serialize.
But json is not configuration, it’s data.
For any serious configuration format,
comments are not optional,
and as soon as we add them,
naive deserialization/serialization breaks.
How can we keep files maintainable for humans,
while enabling automation to update them?

**Split out automation-managed parts.**
One option is to keep the main configuration in a human-friendly format,
and put automation-managed values in separate files.
The automation-managed files don’t need comments,
and we don’t care about preserving formatting,
so they are easy to rewrite.
Some tools natively support splitting configuration across files,
for others we need to merge the files back into a single file.
Templating configuration files is usually a mistake,
but _serializing_ them from a more expressive format can work.
Below we’ll see how RCL enables the human-vs-automation split with imports.

**Hack it with text substitution.**
A little `sed` goes a long way:
regex-match on the old value and replace it.
Because this works on text,
it works for any format,
though it’s generally unsafe.
As with templating configuration files,
it suffers from injection and escaping problems.
Also, reliably locating the value to substitute can be tricky.
Look:

```toml
[kubernetes]
# Be sure to verify that our fleet is on
# a compatible kernel before updating!
version = "1.29.0"

[nginx]
version = "1.29.0"
```

How can we bump Nginx from `1.29.0` to `1.29.1` without touching Kubernetes?
Building a _general_ patching tool based on string matching is madness.
Fortunately, we don’t need to handle _any_ file,
only the configurations that we use in practice,
and those are under our control.
If locating the right `1.29.0` is too hard,
we can add an <code>#&nbsp;auto-update: nginx</code> comment
to the version line to have something to match on.
Text substitution may be a hack,
but it’s a very practical one.

**Syntax-aware editing.**
The correct way to update files is to parse them into a syntax tree,
and perform the edit there.
In order to preserve comments and formatting,
that tree needs to be a _concrete syntax tree_.
Few tools can do this,
but since v0.10.0,
RCL supports it natively through [`rcl patch`][patch].
We’ll see an example below.

**Throwing an LLM at it.**
The 2025 solution:
make an API call to ask Claude to edit the file.
This _can_ work surprisingly well,
<abbr>LLM</abbr>s handle context better than a regex.
They also also add even more failure modes
to the safety and reliability problems of text substitution,
while using orders of magnitude more compute.
It will work most of the time,
but it’s bad engineering,
not a real solution.

## Automation-friendly configuration with RCL

[R<!---->C<!---->L][rcl] is a new configuration language I’m building.
It extends json into a simple functional language
that enables abstraction and reuse.
It’s a principled alternative to templating configuration files,
and enables modularity for tools that don’t natively support it.
Our earlier example, now in RCL:

[rcl]: https://rcl-lang.org/

<pre><code class="sourceCode">{
  <span class="n">kubernetes</span> = {
    <span class="co">// Be sure to verify that our fleet is on</span>
    <span class="co">// a compatible kernel before updating!</span>
    <span class="n">version</span> = <span class="st">"1.29.0"</span>,
  },
  <span class="n">nginx</span> = { <span class="n">version</span> = <span class="st">"1.29.0"</span> },
}
</code></pre>

Running this through `rcl evaluate --format=toml` will produce
the same toml as before, minus the comment.
With [imports], we can split out the automation-managed parts:

<pre><code class="sourceCode"><span class="co">// kubernetes_version.json:</span>
<span class="st">"1.29.0"</span>

<span class="co">// nginx_version.json:</span>
<span class="st">"1.29.0"</span>

<span class="co">// config.rcl:</span>
{
  <span class="n">kubernetes</span> = { <span class="n">version</span> = <span class="kw">import</span> <span class="st">"kubernetes_version.json"</span> },
  <span class="n">nginx</span> = { <span class="n">version</span> = <span class="kw">import</span> <span class="st">"nginx_version.json"</span> },
}
</code></pre>

This evaluates to the same toml,
and now a script can easily rewrite the version files.
It’s powerful,
but the downsides show even in this simple example:
the main configuration grows more complex,
there is a sprawl of small files,
and it’s impossible to see at a glance what versions we are running.
The additional indirection makes `grep` less useful,
and we have no good place to put the warning comment any more.
If we put it in the version file,
that makes the file more difficult to update,
but if we put it in the main configuration,
it would not show up in the diff to remind the reviewer!

To keep the configuration simple
while still enabling automation to edit it,
RCL now features [`rcl patch`][patch],
a built-in way to do syntax-aware editing.
To bump the Nginx version in the original file:

    rcl patch --in-place config.rcl nginx.version '"1.29.1"'

This update is safe:
because `rcl patch` operates on syntax trees,
the result can’t contain syntax errors.
The edit can’t affect how the document parses at all,
except in the intended location.
Because the edit acts on the concrete syntax tree,
it preserves comments and core formatting.
Because RCL is a functional language,
replacing an expression does not cause unintended side effects elsewhere in the document.
And most of all,
we can pinpoint precisely which value to replace.

The patch feature is new in yesterday’s 0.10.0 release,
and is included in the command-line program.
For deeper integration into scripts,
RCL also features a Python module.
I plan to expose the patch functionality there in a future version.

[imports]: https://docs.ruuda.nl/rcl/imports/
[patch]:   https://docs.ruuda.nl/rcl/rcl_patch/

## Conclusion

We want automation to update configuration files.
Doing that safely and reliably is hard for conventional configuration formats.
Splitting configuration into a human-managed and automation-managed part helps,
but adds complexity.
It’s a valid option that RCL supports using imports.
With `rcl patch`,
we can safely automate the rest.
