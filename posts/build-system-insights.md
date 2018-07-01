---
title: Build system insights
date: 2018-06-30
minutes: ?
synopsis: ?
run-in: ?
---

Write some introduction.
Why build systems?

What do I want from a build system?

 * To type a single command to start the build.
   After some time a build artefact should appear.
 * The build artefact does not depend on where or when I perform the build,
   only on the source code checked out
   and flags passed to the build command (such as `--debug`, `--profile` or `-c opt`).
 * When I check out a three-year old commit,
   all of the above should still be true,
   and I should get the same build artefacts that I got three years ago.
 * It should produce the artefact quickly.

Caching and incremental builds
------------------------------

* Fine-grained is better: Scala Dotty does this.
  Incremental compilatin in rustc as well I believe.
  Build systems and toolchains are in conflict here though. How to collaborate?
* Goma caches by hash as well.

**Not reusing old names for new things eliminates the need for cache invalidation
at the cost of requiring garbage collection.
Deterministic names enable shared caches.**
Applied to build systems,
immutability and purity mean that the output path of a build step
should be determined by the input to that build step.
In other words, build artefacts should be stored in an *input-addressable store*.
This has many advantages:

* Different artefacts of a target can coexist in the cache.
  In particular, building is a no-op after checking out a previously built commit
  or after switching to a previously built configuration
  (e.g. enabling and disabling optimisations).
* The cache can safely be shared among multiple repositories.
  A library that is used in two projects need not be built twice.
* The cache can safely be shared among machines.
  Artefacts that CI or a colleague have built already can be fetched from a remote cache.
* There is less need to lock build directories,
  for different builds no longer write to the same directory.

If the path already exists,
the build step can be skipped because the output would be the same anyway.
Ideally the output of a repeated build step [should be bit by bit identical][repro] to the previous output,
but for various reasons the output may be only functionally equivalent.
If the output path does not exist,
it may be obtained from a remote cache rather than by performing the build step.

This insight is not specific to build systems:
the advantages of immutability are *the* key insight of functional programming in general.
Most of [Rich Hickey’s talks][hickey] are an application of this insight,
be it to programming languages, software systems, versioning, or databases.
[Stack realised that dependencies could be shared across repositories.]
Nix and Bazel apply this principle to build systems.
[Goma also works like this.]

Target definitions
------------------

**Build target definitions should live as close to the source code as possible.**
Unlike a global makefile or other build definition in the repository root,
a distributed approach with definitions scattered throughout the repository
remains maintainable even in very large repositories.

This is a lesson I learned from GN and Bazel.
Pants, being inspired by Blaze (Google’s internal version of Bazel) also applies the principle.

**Evaluate build target definitions lazily.**
Lazy evaluation enables good performance even in large repositories,
because only the targets that are actually needed for a build are evaluated.
The majority of build target definitions does not even need to be parsed.

Lazy evaluation of build definitions is a lesson I learned from Nix.
It is what makes installing a package from Nixpkgs fast.
Even though Nixpkgs is an expression that evaluates to
a dictionary of thousands of interdependent packages (build targets),
installing a single package reads very few package definitions from disk.
Guix is an alternative to Nix that uses Scheme to define packages,
rather than Nix’ custom language.
After pulling a new version of GuixSD (the Guix equivalent of Nixpkgs),
Guix spends several minutes compiling package definitions.
In Nix evaluation feels instant.

Bazel applies this principle too by having many `BUILD` files,
and aligning dependency paths with filesystem paths.
Build files of targets that are not depended upon do not need to be loaded.

**Build targets should be fine-grained.**
Having many small targets, rather than fewer large targets,
allows for effective caching and enables parallelisation.
If a change to an input of a target requires rebuilding the entire target,
then making targets smaller reduces the scope of the rebuild.
Furthermore,
a target must wait for all of its dependencies to be built completely
before the target can be built.
If the target actually requires only parts of its dependencies,
then all parts that are not required unnecessarily extend the critical path.
(Something something utilize parallel capacity.
Maybe give Stack example.)

The importance of fine-grained targets is a lesson I learned from Bazel.
Fine-grained targets are the reason that Bazel can build large dependency graphs quickly,
given enough cores.
Ninja is also able to exploid fine-grained dependencies to parallelise the build,
but it relies on the meta build system to generate fine-grained dependencies.
(Modulo C++ special case.)

Ergonomics
----------

**Startup time is important for command-line tools.**
The overhead of interpreters or just in time compilers can be significant.

My experience with Bazel is that although it builds quickly, it is slow to start.
Bazel can take seconds to do a no-op build even in a small repository.
The build tool runs on the JVM, which can achieve good performance at the cost of long warmup times.
To keep startup time manageable,
Bazel has to spawn a daemon that persists between builds,
so the full startup time is paid only once.
Ninja on the other hand is much snappier.
It is a native binary,
and starting quickly was an explicit design goal.

Another good example is the Mercurial source control system.
Its `hg` command is written in Python for extensibility.
This comes at the cost of responsiveness:
just evaluating imports can take a significant amount of time,
relative to executing the command itself.
[Which is why Rust.]

References
----------

 * The [Bazel][bazel] build system, the open source version of Google’s Blaze
 * The [Buck][buck] build system, very similar in spirit to Blaze
 * The [GN][gn] meta-build system, used in Chromium
 * The [Guix][guix] system package manager, inspired by Nix
 * The [Meson][meson] meta-build system
 * The [Ninja][ninja] low-level build system, targeted by GN and Meson
 * The [Nix][nix] system package manager
 * The [Pants][pants] build system, inspired by Blaze
 * The [Shake][shake] build system
 * The [Stack][stack] build tool and language package manager

[bazel]:  https://bazel.build/
[buck]:   https://buckbuild.com/
[gn]:     https://chromium.googlesource.com/chromium/src/+/master/tools/gn/README.md
[guix]:   https://www.gnu.org/software/guix/
[hickey]: https://github.com/tallesl/Rich-Hickey-fanclub
[meson]:  https://mesonbuild.com/
[ninja]:  https://ninja-build.org/
[nix]:    https://nixos.org/nix/
[pants]:  https://www.pantsbuild.org/
[repro]:  https://reproducible-builds.org/
[shake]:  https://shakebuild.com/
[stack]:  https://haskellstack.org/
