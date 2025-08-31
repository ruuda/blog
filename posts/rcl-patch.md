---
title: Automating configuration updates with rcl patch
date: 2025-08-31
lang: en-US
synopsis: TODO
minutes: 999
run-in: Software needs configuration.
teaser: a-float-walks-into-a-gradual-type-system
---

Software needs configuration.
Software is not static, and configurations change over time.
For example,
when we configure a webserver machine,
we may specify the version of Nginx to run,
and bump that when a security update is released.

Initially we write configurations by hand.
When updates are infrequent,
modifying config files by hand is not too bad.
As the number of configurations to manage grows,
and updates become more frequent,
manually opening configuration files in an editor to update them,
becomes tedious and error-prone.
At this point, automation starts to make sense.
For example,
a periodic background job can check for new Nginx releases,
and update the configuration file when a new version is available.
That doesn’t mean _autonomous_ updates
— there can still be a human in the loop.
For example,
if we store the config file an infrastructure-as-code repository,
automation can prepare a pull request to bump the version,
but a human still has to review and accept the change.

## Automation-friendly configuration

When automation has to update configuration,
we face a challenge:
formats that are pleasant for humans to read and write,
are hard for automation to modify,
and formats that are easy for automation to process,
limit expressivity.
Safely updating a json file is easy enough:
deserialize it,
modify the data structure,
and serialize it again.
But json is more data than configuration:
it doesn’t have comments,
and it has more line noise than e.g. toml.
Even with automation,
most of the configuration will still be written by humans,
but more importantly,
_read by humans_.
Comments and formatting matter!

We can resolve the tension in three ways:

**Split out automation-managed parts.**
Keep the main configuration in a human-friendly format,
and put automation-managed values in separate files.
The automation-managed files don’t need comments,
and we don’t care about preserving formatting,
so they are easy to rewrite.
Some tools natively support multiple configuration files,
but often we need a way to merge the separate files
back into a single configuration.
Templating configuration files is rarely a good idea,
but _generating_ configuration files is an option.
Below we’ll see how RCL enables
the human-managed vs. automation-managed split using imports.

**Hack it with string substitution.**
A little `sed` goes a long way:
regex-match on the old value and replace it with the new value.
Because we operate on text,
this in principle works for any format,
though it’s not always safe.
As with templating configuration files,
it can be susceptible to injection and escaping problems.
Furthermore,
reliably locating the value to substitute can be tricky.
Consider the following file:

```toml
[nginx]
version = "1.29.0"

[kubernetes]
version = "1.29.0"
```

How can we bump Nginx from `1.29.0` to `1.29.1`
without accidentally updating Kubernetes instead?
Building a general patching tool based on string matching is hard,
but usually we don’t need to handle _any_ file,
just the configurations in our repository,
that we control anyway.
If locating the right `1.29.0` is too hard,
we can add an <code>#&nbsp;auto-update: nginx</code> comment
to the version line to have something to match on.
So while text substitution is a hack,
it is a very practical one.

**Syntax-aware editing.**
If text substitution is a hack,
the proper way to update files would be to parse them into a syntax tree,
and perform the edit there.
In order to preserve comments and formatting,
that tree needs to be a _concrete syntax tree_.
