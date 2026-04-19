---
title: Building the <br> deployment tool <br> I wish I had
date: 2026-04-19
lang: en-US
synopsis: TODO write a synopsis
minutes: ?
run-in: It is 00:43 at night.
---

It is 00:43 at night.
I look at the plan and press y.
“s4.ruuda.nl: connecting, …, done” flashes through my terminal window.
It worked.
Now the real work can start.
_Pop a stack frame._
What was I doing again?

People who have worked with me for a while
might accuse me of suffering from not-invented-here syndrome.
I’d say I just have higher standards.
Why subject yourself to endless frustration
using somebody else’s mediocre tools,
when you can build your _own_ tools that are actually _nice_ to use?
Anyway, what was I doing again?

Right, I wanted to write a blog post about digital sovereignty.
It would be hypocritical to publish that
on a blog hosted in the US,
with a US-controlled hyperscaler.
So let’s move that to Europe, easy enough.
_Push stack frame._
Spawn a new VM, point the DNS records at … oh, DNS.
Right.
I use Cloudflare for that.
Another entity that Trump can order to stop providing services here when shit hits the fan.
Maybe I should self-host my DNS servers then.
_Push another stack frame._

My webserver is a tiny VM that runs Nginx,
plus Lego to renew certificates.
That Nginx configuration grew somewhat complex
as I acquired domains throughout the years,
but I generate it with Nix so it’s fine,
just two configuration files.
I wrote a small Python script
that copies the files to the server and restarts Nginx.
[Ansible? Nein danke!](https://social.hackerspace.pl/@q3k/110598898979332486)
The script served me well for the past years.
But now I want to start running NSD.
I need at least _two_ servers now.
And even more systemd units,
a configuration file,
zonefiles …
That script is not going to cut it any more.

“NixOS”
I hear [Arian](https://functional.cafe/@arianvp)’s voice whisper in my head.
“Just use NixOS.
It’s only one line to configure,
`services.nsd.enable = true`.”
He’s right of course.
I already use Nix to build minimal EROFS images for Nginx and Lego,
that’s how I run them on Flatcar.
But I like Flatcar as the OS,
and I prefer running my services in minimal readonly chroots.
Let’s not switch distros right now.
Let’s build a new deployment tool instead.
_Push stack frame_.

## Building the agent

Static binary, simple install command.

## The store

Git repo, directory per host, generated externally through RCL.

## Running a deployment

Connect, lock, push, apply, report systemctl status output back.

## Conclusion

It works, its very nice to use.
