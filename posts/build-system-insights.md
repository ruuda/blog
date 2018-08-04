---
title: Build system insights
date: 2018-06-30
minutes: ?
synopsis: ?
run-in: ?
---

A new generation of build systems has been gaining popularity over the past few years,
joining the already plentiful collection of build tools.
Although these new build systems differ in origin and purpose,
there are common themes to them.

Lately I ended up interacting with many different build systems,
which got me thinking about the topic.
I started to realise that there are a few key principles
that underlie a good build system.
In this post I want to lay out some of the insights
that changed my view about how build tooling should work.

The line between build tool and package manager became fuzzy.

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

A note on taxonomy:
in this post I refer to [Bazel][bazel] a few times,
the open-source version of Google’s Blaze.
Before Bazel was published,
ex Googlers at other companies created Blaze clones,
resulting in [Pants][pants], [Buck][buck], and [Please][please].
Most of the discussion about Bazel applies equally well to these other build systems.

Caching and incremental builds
------------------------------

If you take away one thing from this post,
let it be this:

**Not reusing old names for new things eliminates the need for cache invalidation
at the cost of requiring garbage collection.
Deterministic names enable shared caches.**

Applied to build systems,
this means that the output path of a build step
should be determined by the input to that build step.
In other words, build artefacts should be stored in an *input-addressable store*.
If the path already exists,
the build step can be skipped because the output would be the same anyway.
Ideally the output of a repeated build step
[should be bit by bit identical][repro] to the previous output,
but for various reasons the output may be only functionally equivalent.
If the output path does not exist,
it may be obtained from a remote cache rather than by performing the build step.
Caching rolls out naturally:

* Different artefacts of a build target can coexist in a cache.
  In particular,
  building is a no-op
  when building a previously built revision
  (for example when switching between topic branches)
  or configuration
  (such as enabling and then disabling optimisations).
* The cache can safely be shared among multiple unrelated repositories.
  A library that is used in two projects need not be built twice.
* The cache can safely be shared among machines.
  Artefacts that continuous integration
  or a colleague have built already
  can be fetched from a remote cache.

The advantages of immutability and pure functions
are not specific to build systems:
I would argue that they are the key insight
of functional programming in general.
<!--
Most of [Rich Hickey’s talks][hickey] are an application of this insight,
be it to programming languages, software systems, versioning, or databases.
-->
Most modern build tools
use an immutable input-addressable cache
in one way or another.
[Nix][nix] applies the technique to system package management,
[Bazel][bazel] to fine-grained build targets.
[Stack][stack] realised that dependencies could be shared across repositories.
[Goma][goma] caches build artefacts based on hashes of input files and the exact compile command.
Treating build steps as pure functions makes caching a breeze.

The major caveat is that it is difficult to capture *all* input to a build step.
Many toolchains implicitly capture state from the environment,
for instance by reading the `CXX` environment variable or discovering include paths.
Indeed, Nix and Bazel go to great lengths
to prevent accidentally capturing this state,
and to provide a controlled and reproducible environment
for toolchains that inevitably capture state.

The idea of using an immutable store for caching
does not need to stop at the module level.
A compiler can be thought of as a build system,
where intermediate representations of functions
or even individual expressions form a graph of build targets.
Incremental compilation and the possibility of a responsive [language server][lngsrv]
fall out naturally from this point of view.
Scala’s Dotty compiler [implements this idea][dotty].
Incremental compilation in Rust [is also based][rustc] on [lazy functional][rustc2] graph caching,
although its cache is neither immutable nor iput-addressable,
and requires invalidation.
In any case,
caching the output of pure functions in an immutable input-addressable store
is a powerful concept
at all levels of abstraction:
from compiler internals to building modules,
libraries,
and even entire system packages.

Toolchains and dependencies
---------------------------

**The build tool should manage the compiler toolchain.**<br>
When a toolchain or other dependency needs to be obtained externally,
building devolves from a single-step command
into hours of dependency hunting and configuration fiddling.
Language package managers make it easy to obtain language dependencies,
but they often stop right there.
When a readme informally specifies the toolchain,
rather than a machine-enforcable build definition,
reproducibility suffers.

The build tool that made me realise the importance
of a build tool-managed compiler was [Stack][stack],
a build tool for Haskell.
Managing the compiler means
that I can check out a two-year old commit of [my blog generator][src],
and `stack build` still produces a binary.
Rust’s version manager Rustup learned this lesson recently
with the [introduction][rustup] of a toolchain file.

Managing the compiler also removes
much of the need for a traditional `configure` script.
Libraries may want to support multiple toolchains,
but for binaries a lot of complexity can be avoided.

Try that on a two-year old [Rust project][convector] of mine:
`cargo build` no longer works.
(Was able to fix after an hour of trying toolchains from around that time ... I never wrote down the exact version.)

When checking out v1.0.0 of a [Rust project][hound] of mine (released three years ago),
a plain `cargo build` no longer works,
because Rust chose to have an external “version manager” called Rustup
on top of the build tool Cargo,
and at that time Rustup did not support pinning the toolchain.

Haskell libraries tend to put upper bounds
on the version of the standard library (shipped with the compiler).
Using a compiler from the system package repositories
constrains the set of usable libraries significantly,
and furthermore,
your code might stop comiling suddenly.

If you use a compiler from the system package repositories,

Also, pinning.
Hermeticity is one reason.
Pinning another.
The tools that do this are the only ones that "just work" (i.e. Nix).

Learned from Stack (vs Cargo) at first,
became very clear with Nix.

**An exact toolchain version should be pinned as part of the build target definition.**
Because reproducibility.
Mention Stack vs Cargo example.
Also, if using system package manager,
code stops compiling suddenly.

Learned from Stack, I think.
Bazel can do it.

Target definitions
------------------

**Build target definitions should live as close to the source code as possible.**<br>
Unlike a global makefile or other build definition in the repository root,
a distributed approach with definitions scattered throughout the repository
remains maintainable even in very large repositories.

This is a lesson I learned from Chromium’s build system [GN][gn], and from Blaze.
The Blaze derivatives Pants, Buck, and Please
also apply this principle,
as did GN’s predecessor [GYP][gyp].

**Evaluate build target definitions lazily.**<br>
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

**Build targets should be fine-grained.**<br>
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
Ninja is also able to exploit fine-grained dependencies to parallelise the build,
but it relies on the meta build system to generate fine-grained dependencies.
(Modulo C++ special case.)

Ergonomics
----------

**Startup time is important for command-line tools.**<br>
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

References and further reading
------------------------------

Tools mentioned throughout this post:

 * The [Bazel][bazel] build system, the open source version of Google’s Blaze
 * The [Buck][buck] build system, inspired by Blaze
 * The [GN][gn] meta-build system, used in Chromium
 * The [GYP][gyp] meta-build system, the predecessor to GN
 * The [Guix][guix] system package manager, inspired by Nix
 * The [Meson][meson] meta-build system
 * The [Ninja][ninja] low-level build system, targeted by GN and Meson
 * The [Nix][nix] system package manager
 * The [Pants][pants] build system, inspired by Blaze
 * The [Please][please] build system, inspired by Blaze
 * The [Shake][shake] build system
 * The [Stack][stack] build tool and language package manager

Further reading:

 * [Build Systems à la Carte][carte]
   by Andrey Mokhov, Neil Mitchell, and Simon Peyton Jones,
   classifies build tools based on their rebuilding strategy and scheduling algorithm.
 * [Compilers are Databases][dotty], a talk by Martin Odersky,
   explains how functional reactive programming can be applied to compilers.
 * [Houyhnhnm Computing Chapter 9: Build Systems][ngnghm],
   part of an insightful series that reevaluates computing from a hypothetical alien perspective,
   argues that functional reactive programming is a good fit for build systems.

[bazel]:  https://bazel.build/
[buck]:   https://buckbuild.com/
[carte]:  https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/
[dotty]:  https://www.youtube.com/watch?v=WxyyJyB_Ssc
[gn]:     https://chromium.googlesource.com/chromium/src/+/master/tools/gn/README.md
[goma]:   https://chromium.googlesource.com/infra/goma/client
[guix]:   https://www.gnu.org/software/guix/
[gyp]:    https://gyp.gsrc.io/
[hickey]: https://github.com/tallesl/Rich-Hickey-fanclub
[lngsrv]: https://microsoft.github.io/language-server-protocol/
[meson]:  https://mesonbuild.com/
[ngnghm]: https://ngnghm.github.io/blog/2016/04/26/chapter-9-build-systems/
[ninja]:  https://ninja-build.org/
[nix]:    https://nixos.org/nix/
[pants]:  https://www.pantsbuild.org/
[please]: https://please.build/
[repro]:  https://reproducible-builds.org/
[rustc2]: https://github.com/nikomatsakis/rustc-on-demand-incremental-design-doc/blob/e08b00408bb1ee912642be4c5f78704efd0eedc5/0000-rustc-on-demand-and-incremental.md
[rustc]:  https://blog.rust-lang.org/2016/09/08/incremental.html
[shake]:  https://shakebuild.com/
[src]:    https://github.com/ruuda/blog
[stack]:  https://haskellstack.org/
[rustup]: https://github.com/rust-lang-nursery/rustup.rs/commit/107d8e5f1ab83ce13cb33a7b4ca0f58198285ee8
