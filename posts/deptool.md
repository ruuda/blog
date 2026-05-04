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

Right, I wanted to write a blog post about European digital sovereignty.
It would be hypocritical to publish that
on a blog hosted in the US,
with a US-controlled hyperscaler.
So let’s move that to Europe, easy enough.
_Push stack frame._
Spawn a new VM, point the DNS records at … oh, right, DNS.
I use Cloudflare for that.
Another entity that Trump can order to stop providing services when shit hits the fan.
Maybe I should self-host my DNS servers then.
_Push stack frame._

My webserver is a tiny VM that runs Nginx,
plus Lego to renew certificates.
The Nginx configuration grew somewhat complicated over the years,
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

[miniserver]: https://codeberg.org/ruuda/miniserver

## Wishlist

So, if I’m going to build my own tool …
what would an actually _nice_ configuration management tool look like?
Here we can look to Ansible for guidance:
it made all the mistakes so others can learn from them.
I want my tool to be:

**Fast.**
A configuration update should be sub-second.
There’s no _fundamental_ reason for it to be slower than that,
even a transatlantic ping is only 100ms.

**Predictable.**
The tool should show me what it’s going to do,
and then do just that.
Like OpenTofu, with a separate _plan_ and _apply_ phase.
Not like Ansible,
where check mode is useless
because every imperative step can trigger
a cascade of changes that are only known after executing the step.
And where nothing prevents the host from changing
between the check and the real run,
making the check more of a vibe check
than something you can depend on.

**Safe.**
If I break my Nginx configuration,
I don’t want my webserver to be down for minutes
while I frantically try to fix it.
(Me: “Ah, only a small typo,
faster to just fix it than to try and restore the previous version.”
Narrator: “If only that typo were the _only_ problem …”)
No, I want the tool to automatically roll back for me.
In milliseconds.

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
and end up with drift and lingering files when I inevitably forget that.


**Zero-setup.**
I want to use this tool to manage my servers right after provisioning them.
I don’t want to manually install agents, daemons, or their dependencies,
and I don’t want to have to enroll or register the host anywhere,
because then I’d have the new problem of automating _that_.

## Decouple distribution

The core idea feels obvious in hindsight,
but it was inspired by lots of prior art.
At work, [David][david] had recently built [Unsible][unsible],
a tool that takes Ansible playbooks,
but instead of executing them step by step,
it locally builds a tarball and ships it to the host,
where it mostly just needs to put the files in place.
I like this idea of decoupling configuration generation from distribution.
In a sense this is also what NixOS does.
[NixOps][nixops] applied it to clusters.
We can learn more from Nix:
store generated artifacts in a place where different versions can coexist,
and limit the imperative parts of system administration
to a small activation step that swaps a few symlinks.
This design works well both for package management and system configuration.

[unsible]: https://github.com/ChorusOne/unsible
[david]:   https://davidv.dev/
[nixops]:  https://github.com/NixOS/nixops

## Hello Deptool

With the scope reduced
to distributing small files and running a simple activation step,
deployment becomes a tractable problem to solve.
I call my take on it [Deptool][deptool].
Here’s how it works.

**Pre-render config files for the entire cluster.**
Store them in a directory on disk.
This directory tree is two levels deep:
a directory per target host at the top level,
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
In there we create a directory named after the commit to deploy.
Now multiple versions can coexist on disk.
Then we point a `current` symlink at the deployed version.
Now we can swap versions atomically,
and we never have lingering files.
Deleted files are simply not materialized in a next version.
For applications that demand files in particular locations,
we can create symlinks to `/var/lib/deptool` anywhere on the filesystem.
That step is not atomic,
but it only has to happen when adding or removing symlinks,
not when we edit the files they point to.
And if we later deploy a version that no longer includes the symlink,
we know from the diff that we need to delete it.
Nothing lingers.

**Track what is deployed with remote-tracking refs.**
Back on the operator laptop,
we track the commit we deployed to each host.
Now we can compute the cluster diff offline.
This diff is the deployment plan,
and we can present it in _milliseconds_.

**To deploy, first try to take a lock at the target host.**
We connect over SSH and send a lock request.
In the request we include the commit that we think is deployed on that host.
If we get the lock,
then our plan was valid,
and nothing else can be deployed on that host until we release the lock,
so the plan _remains_ valid.
Deployment only proceeds
if we hold the lock for all hosts affected by a change.
If any ref was outdated and something else is deployed on a host,
that means the plan is _stale_,
and we abort.
We update our local refs,
and the next run will show an up to date plan.

**Restart systemd units.**
I run all my services as systemd units,
and they are quick to start,
so I can err on the side of restarting them when in doubt.
After an application’s configuration changed,
we restart the affected systemd units.
If the unit fails to start,
we can point the symlink back at the previous known-good version,
and restart again.
Automatic rollback in milliseconds.

So this is the core idea behind Deptool.
Cluster configuration is just a directory tree tracked in a Git repository,
and on each host we check out the relevant files into isolated directories,
change a few symlinks,
and restart systemd units.
That’s it!

[deptool]: https://docs.ruuda.nl/deptool/

## Optimistic concurrency

A Deptool deployment has elements of optimistic concurrency.
We assume that we know the current cluster state
and plan based on that.
If the assumption turned out wrong, we have to retry.
This means it’s very fast when there is no contention
(when I’m the only person deploying, always from the same laptop),
at the cost of performing abysmally under contention
(a team of people all trying to deploy constantly,
where only one can win and everybody else has to retry).
This is the same model as `git push`.
It’s not going to scale to hundreds of people or thousands of servers,
but that’s okay.
I’m the only person managing my personal infra,
and I don’t have thousands of servers.
The nice thing about building your own tools
is that you can optimize them for exactly your own use case!

[neindanke]: https://social.hackerspace.pl/@q3k/110598898979332486

## Building the agent

We have the design,
we know on a high level what to do.
But how do we make it happen on the managed host?
My webserver runs Flatcar Linux,
an image-based OS with a very minimal userspace.
It has coreutils and Bash,
but no package manager and no Python.
This is great for reducing attack surface.
It’s not so great for installing things.
I want to use my tool to manage my servers,
so it needs to be compatible with Flatcar out of the box.
If I need to install something for the tool to work,
then I’d have the new problem of automating _that_ installation process!

So we need to manage the fresh host from the outside.
I can SSH into it, and use passwordless sudo there.
Maybe we can just execute commands over SSH?
If you have ever tried to automate anything mildly nontrivial that way,
you will have quickly learned the hard way
that this is not just slow
— the argv also doesn’t cross the SSH boundary unscathed.
I want a fast tool and I don’t want to deal with the subtleties
of word splitting and escaping shell-over-SSH.
So let’s sidestep this minefield.
How about we run _one_ simple command?
A program that takes zero args, at a predictable location.
This is the _agent_.
The agent reads messages from stdin, and responds on stdout.
We use SSH only for transport, as a socket.
Now we have code execution on the host,
and we can send whatever input we want,
without ever having to worry about escaping!

Now we just need to start the agent …
But how did the agent get there?
This is a fresh host!
And how do we even build this agent?
Here too we can look to Ansible for mistakes to avoid.
After you [mitigate][mitogen] its worst flaws,
Ansible still sends over megabytes of Python modules
_every single time it connects_ to a host.
Needless to say, this is slow.
Excruciatingly slow.
The one nice thing about this design
is that you know exactly what’s running on the remote end,
you never have version mixups.
In theory, at least.
In practice I’ve wasted more hours than I dare to admit
staring at Python module errors,
because Ansible still has requirements about the host,
it’s not _fully_ self-contained.
That creates a compatibility nightmare.
We don’t even _have_ Python on Flatcar.
So how about this instead?

**We build a static binary.**
No assumptions about what’s available except the kernel,
and no interpreter that has to parse megabytes of code
before it can execute anything.

**We put it at a location named after the commit it was built from.**
This ensures that both ends of the connection run the same version,
so there are never any issues with wire format compatibility.
If we managed to start the binary, it speaks the right protocol.
We’ll put it at `/var/lib/deptool/bin/deptool-<version>-<commit>`.

**We optimistically assume the binary is already there.**
An SSH handshakes is fairly expensive.
Let’s not waste any on probing or on idempotent installation steps.
The binary is about 1.6 MB.
Not prohibitively slow to send,
but not free either,
and cluster configuration changes far more frequently than Deptool;
the common case is that the binary exists.

If we fail to start the binary, then we need to install it.
We connect over SSH a second time,
now with this command:

    uname -sm
    && sudo mkdir -p /var/lib/deptool/{bin,apps,store}
    && sudo dd status=none of=<remote_bin_path>
    && sudo chmod +x <remote_bin_path>
    && sudo sha256sum <remote_bin_path>

We drive that like so:
First, we read one line from stdout to get the `uname` output.
This tells us the machine’s OS and CPU architecture,
so we can send the appropriate agent binary for that platform.
We write that binary to stdin;
`dd` writes it to disk on the remote end.
Finally, we read one more line from stdout:
the remotely computed shasum.
This confirms that the transfer was successful.
When we retry starting the agent now, it should succeed.
The agent cleans up old versions to avoid filling up the disk.

Let’s recap.
What do we have?

 * A way to execute an agent on a remote host
   and communicate with it over a socket.
 * All user-controlled input goes over our SSH-backed socket,
   it never enters SSH or shell commands,
   so we sidestep any escaping issues.
 * Both sides run the same version,
   the protocol is compatible by construction.
 * This costs only a single SSH handshake in the common case,
   so latency is minimal.
 * In the uncommon case
   (deploying against a new machine, or after updating the tool)
   it’s two additional connects plus a one-time 1.6 MB transfer.
   With `ControlMaster` we can skip most of the overhead on the later connections,
   so overall it costs a few seconds.
   No sub-second deploy in this case,
   but still better than Ansible.
 * Automatic installation that requires nothing beyond coreutils
   on the remote host.

This approach has been working out great for me
even as I was building Deptool,
and the uncommon case of having to copy the binary was quite common.
Especially for the case where I deploy a configuration,
tweak it a bit,
and deploy again,
SSH can preserve the underlying connection,
so it really feels instant.

[mitogen]:   https://mitogen.networkgenomics.com/ansible_detailed.html

## Conclusion

It works, its very nice to use.
Write about how sub-second deploy is a game-changer,
no temptation to edit files in prod.
