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
The script served me well for the past years,
but now I want to start running DNS servers.
I need at least _two_ servers now.
And more systemd units, configuration files, zonefiles …
The script is not going to cut it any more,
I need serious cluster configuration management.
_Push stack frame._

“NixOS”
I hear [Arian](https://functional.cafe/@arianvp)’s voice whisper in my head.
“Just use NixOS.
It’s only one line to configure,
`services.nsd.enable = true`.”
He’s right of course.
I already use Nix to [build][miniserver] minimal EROFS images for Nginx and Lego.
That’s how I run them on Flatcar.
But I like the idea of a minimal base OS,
and running my services from readonly chroots with no more binaries than needed.
“Let’s not scope-creep this into switching distros right now,”
I tell myself.
“Let’s build a new deployment tool instead.”
_Push stack frame._

[miniserver]: https://codeberg.org/ruuda/miniserver

## We can have nice things

So, if I’m going to build my own tool …
what would an actually _nice_ configuration management tool look like?
Here we can look to Ansible for guidance:
it made all the mistakes so others can learn from them.
I want my tool to be:

**Fast.**
A configuration update should be sub-second.
There’s no _fundamental_ reason for it to be slower than that,
even a transatlantic ping is only 100ms.

**Safe and predictable.**
The tool should show me what it’s going to do,
and then do it.
Like OpenTofu, with a separate _plan_ and _apply_ phase.
Not like Ansible,
where check mode is useless
because every imperative step can trigger
a cascade of changes that are only known after executing the step.
And where nothing prevents the host from changing
between the check and the real run,
making the check more of a vibe check
than something you can depend on.

**Simple.**
I just need to copy configuration files
from my laptop to my servers
and restart a few systemd units.
I don’t need to solve every deployment problem for everybody;
I don’t need control flow or arbitrary code execution.
I do need to be able to template
— excuse me, [_generate_][yaml-generate]
— configuration files,
but a [separate tool][rcl-build] can do that.

[rcl-post]:      /2024/a-reasonable-configuration-language/
[yaml-generate]: /2023/01/11/the-yaml-document-from-hell/#templating-yaml-is-a-terrible-terrible-idea
[rcl-build]:     https://docs.ruuda.nl/rcl/generating_files/

**Declarative.**
If I remove a file or application from my config,
it should be removed from the server.
I don’t want to have to add explicit cleanup steps,
and end up with drift and lingering files when I inevitably forget to do so.

**Zero-setup.**
I want to use this tool to manage my servers right after provisioning them.
I don’t want to manually install agents, daemons, or their dependencies,
and I don’t want to have to enroll or register the host anywhere,
because then I’d have the new problem of automating _that_.

## Decoupling generation from distribution

I’ve been thinking about and tinkering with deployment systems
for a long time in different settings.
At work, [David][david] had recently built [Unsible][unsible],
a tool that takes Ansible playbooks,
but instead of executing them step by step,
it locally builds a tarball and ships it to the host,
where it mostly just needs to put the files in place.
I like this idea of decoupling configuration generation from distribution.
In a sense this is also what NixOS does.
We can learn more from NixOS:
store generated artifacts in a store where different versions can coexist,
and limit the imperative parts of system administration
to a small activation step that swaps a few symlinks.
Good deployment tools invariably gravitate towards this design.

[unsible]: https://github.com/ChorusOne/unsible
[david]:   https://davidv.dev/

## Enter Deptool

With the scope reduced
to distributing small files and running a simple activation step,
deployment becomes a tractable problem to solve.
I called my deployment tool _Deptool_.
Here’s how it works.

**Pre-render config files for the entire cluster.**
We store them in a directory on disk.
This directory tree is two levels deep:
a directory per target host at the top level;
a directory per application below that.

**Put that in a Git repository.**
Now we can diff one version against another,
and see what changed across the entire cluster.
From the diffstat we can see which hosts are affected
and which apps changed there,
and for every config file we have a precise diff.

**Materialize files in an isolated directory on the hosts.**
We put everything in `/var/lib/deptool`
where it doesn’t interfere with anything.
In there we create a directory per application
and name it after the commit.
Now multiple versions can coexist on disk.
Then we point a `current` symlink at the deployed version.
Now we can swap versions atomically,
and we never have lingering files.
Deleted files are simply not materialized in a next version.
For applications that demand files in particular locations,
we can create symlinks to `/var/lib/deptool`.
If we deploy a later version that does not have this symlink,
we know from the diff that we need to delete it;
nothing lingers.

**Track what is deployed with remote-tracking refs.**
Back on the operator laptop,
we track the commit we deployed to each host.
Now we can compute the cluster diff offline.
This diff is the deployment plan,
and we can present it in _milliseconds_.

**To deploy, we connect to the target host and try to take a lock there.**
In the lock request
we include the commit that we think is deployed on that host.
If we get the lock,
then our plan was valid,
and nothing else can be deployed on that host until we release the lock,
so the plan _remains_ valid.
Deployment can only proceed
if we hold the lock for all hosts affected by a change.
If any ref was outdated and something else is deployed on a host,
that means the plan is _stale_,
and we abort.
We update our local refs,
and the next run will show an up to date plan.

A Deptool deployment has elements of optimistic concurrency.
As with optimistic concurrency in other systems,
this means it’s very fast when there is no contention
(when I’m the only person deploying, always from the same laptop),
at the cost of performing abysmally under contention
(a team of people all trying to deploy constantly,
where only one can win and everybody else has to retry).
It don’t expect it to scale to hundreds of people or thousands of servers,
but that’s okay.
I’m the only person managing my personal infra,
and I don’t have thousands of servers.

So this is the core idea behind Deptool.
Cluster configuration is just a directory tree tracked in a Git repository,
and on each host we check out the relevant files into isolated directories.

[neindanke]: https://social.hackerspace.pl/@q3k/110598898979332486

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

<!-- TODO: Mention Git packfile transport hack. -->

## Running a deployment

Connect, lock, push, apply, report systemctl status output back.

## Conclusion

It works, its very nice to use.
