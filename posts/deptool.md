---
title: Building the deployment tool I wish I had
break: deployment tool
date: 2026-04-19
lang: en-US
synopsis: TODO write a synopsis
minutes: ?
run-in: It is 00:43 at night.
---

It is 00:43 at night.
I look at the plan and press y.
“s4.ruuda.nl: connecting …”
I hold my breath.
“Applying” briefly flashes in my terminal window before settling on “done.”
It worked.
Now the real work can start.
_Pop stack frame._
What was I doing again?

People who have worked with me for a while
might accuse me of suffering from not-invented-here syndrome.
I prefer to call it “having higher standards.”
Why subject yourself to endless frustration
that mediocre tools inflict on you,
when you can build your _own_ tools that are actually _nice_ to use?
Anyway. What was I doing again?

Right, I wanted to write a blog post about digital sovereignty.
It would be hypocritical to publish that
on a blog hosted in the US,
with a US-controlled hyperscaler.
So let’s move that to Europe, easy enough.
_Push stack frame._
Spawn a new VM, point the DNS records at … oh, right, DNS.
I use Cloudflare for that.
Another entity that Trump can order to stop providing services here when shit hits the fan.
Maybe I should self-host my DNS servers then.
_Push stack frame._

My webserver is a tiny VM that runs Nginx,
plus Lego to renew certificates.
The Nginx configuration grew somewhat complex over the years,
but I generate it with Nix so it’s fine,
just two configuration files.
I wrote a small Python script
that copies the files to the server and restarts Nginx.
The script served me well for the past years.
But now I want to start running NSD.
I need at least _two_ servers now.
And more systemd units, configuration files, zonefiles …
The script is not going to cut it any more.


“NixOS”
I hear [Arian](https://functional.cafe/@arianvp)’s voice whisper in my head.
“Just use NixOS.
It’s only one line to configure,
`services.nsd.enable = true`.”
He’s right of course.
I already use Nix to build minimal EROFS images for Nginx and Lego,
that’s how I run them on Flatcar.
But I like the idea of a minimal base OS,
and running my services from readonly chroots with no unneeded binaries.
“Let’s not scope-creep this into switching distros right now,”
I tell myself.
Let’s build a new deployment tool instead.
_Push stack frame_.

## Building the agent

My webserver runs Flatcar Linux,
an image-based OS with very minimal userspace and no package manager.
It has coreutils and Bash, but not even Python.
This is great for reducing attack surface.
It’s not so great for installing things.
I want to use my tool to manage my servers,
so it needs to be compatible with Flatcar out of the box.
If I need to install something for the tool to work,
then I’d have the new problem of automating _that_ installation process!

So we need to manage the fresh host from the outside.
I can SSH into it, and use passwordless sudo thore.
Can we just execute commands over SSH?
If you have ever tried to automate anything mildly nontrivial that way,
you will have quickly learned the hard way
that the argv doesn’t cross the SSH boundary unscathed.
I want to spend my time configuring my server,
not figuring out the subtleties of
word splitting and escaping in shell-over-SSH.
So let’s sidestep this minefield.
How about we run _one_ simple command?
A program that takes zero args, at a predictable location.
This is the _agent_.
The agent reads messages from stdin, and responds on stdout.
We use SSH as if it’s a socket.
A json per line is not the most efficient, but it will do for now.
Now we can send whatever input we want,
without ever having to worry about escaping!

So, we just need to start the agent …
But how did the agent get there?
This is a fresh host!
Also, how do we build this agent?

Here we can look to Ansible for guidance:
it made all the mistakes so we don’t have to.
Making assumptions about the host,
especially about Python versions and available modules,
creates a compatibility nightmare.
Sending over megabytes of dependency modules
_every single time we connect_
makes things excruciatingly slow,
especially if a deployment involves doing that repeatedly against the same host,
and paying the Python startup overhead every time.
Even with [a third-party plugin][mitogen]
that mitigates the worst mistakes,


So how about this:

 1. We build a static binary.
    No assumptions about what’s available except the kernel.
    No interpreter startup overhead.
 2. We put it at a location named after the commit it was built from,
    so there are never any issues with wire format compatibility.
    If we managed to start the binary, it speaks the right protocol.
    We’ll put it at `/var/lib/deptool/bin/deptool-<commit>`.
 3. We optimistically assume the binary is already there:
    SSH handshakes are expensive,
    so let’s not waste any on probing
    or on idempotent installation steps.

If we fail to start the binary, then we need to install it.
We connect over SSH a second time,
now with this command:

    uname -sm
    && sudo mkdir -p /var/lib/deptool/{bin,apps,store}
    && sudo dd status=none of=<remote_bin_path>
    && sudo chmod +x <remote_bin_path>
    && sudo sha256sum <remote_bin_path>

We drive this session as follows:

 * Read one line from stdout to get the `uname` output.
   This tells us the machine’s OS and CPU architecture,
   so we can send the appropriate agent binary for that platform.
 * Write the binary to stdin, `dd` writes it to disk on the remote end.
 * Read one more line from stdout,
   the remotely computed shasum,
   to confirm that the transfer was successful.

After that,
we retry starting the agent,
which should suceed now.

So, what did we gain?

 * A way to execute a program on a remote host,
   and communicate with it pretty much like a socket.
 * This costs only a single SSH handshake in the common case,
   so latency is minimal.
 * In the uncommon case
   (deploying against a new machine, or after updating the tool)
   it’s 2 additional connects plus a one-time 1.6 MB transfer,
   though with `ControlMaster` we can skip most of the overhead
   on the later connections.
 * All user-controlled input goes over our SSH-backed socket,
   it never enters SSH or shell commands,
   so we sidestep any escaping issues.

[mitogen]:   https://mitogen.networkgenomics.com/ansible_detailed.html
[neindanke]: https://social.hackerspace.pl/@q3k/110598898979332486

## The store

Git repo, directory per host, generated externally through RCL.

## Running a deployment

Connect, lock, push, apply, report systemctl status output back.

## Conclusion

It works, its very nice to use.
