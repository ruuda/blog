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
