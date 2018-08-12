---
title: Build system insights
date: 2018-06-30
minutes: ?
synopsis: ?
run-in: A new generation of build systems
extra-glyphs: 1234567890
---

A new generation of build systems has been gaining popularity over the past few years,
joining the already plentiful collection of build tools.
Although these new build systems differ in origin and purpose,
there are common themes to them.
Lately I ended up interacting with many different build systems,
which got me thinking about the topic.
Slowly a few key principles that underlie a good build system emerged.
In this post I want to lay out some of the insights
that changed my view about how build tooling should work.

A note on taxonomy:
in this post I refer to Bazel a few times,
the open-source version *Blaze*, Google’s internal build system.
Before Bazel was published,
ex Googlers at other companies created Blaze clones,
resulting in *Pants*, *Buck*, and *Please*.
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
[Bazel][bazel] and [SCons][scons] to fine-grained build targets.
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
although its cache is neither immutable nor input-addressable,
and requires invalidation.
In any case,
caching the output of pure functions in an immutable input-addressable store
is a powerful concept
at all levels of abstraction:
from compiler internals to building modules,
libraries,
and even entire system packages.

Target definitions
------------------

**Build target definitions should live as close to the source code as possible.**<br>
Unlike a global makefile or other build definition in the repository root,
a distributed approach with definitions placed throughout the repository
remains maintainable even in very large repositories.

This is a lesson I learned from Chromium’s build system [GN][gn], and from Blaze.
The Blaze derivatives Pants, Buck, and Please
also apply this principle,
as did GN’s predecessor [GYP][gyp].

**Build targets should be fine-grained.**<br>
Having many small targets, rather than fewer large targets,
allows for effective caching and enables parallelisation.
If a change to an input of a target requires rebuilding the entire target,
then making targets smaller reduces the scope of that rebuild.
Targets that do not depend on eachother can be built in parallel,
therefore finer targets generally means more parallelism.
Furthermore,
a target must wait for all of its dependencies to be built completely
before the target can be built.
If the target uses only a small part of a dependency,
then the unused parts unnecessarily prolong the critical path.
Given enough CPU cores,
fine-grained targets can build significantly faster than coarse targets.

![CPU occupation during build with coarse and fine-grained targets.](/images/build.svg)

The above graph shows a concrete example of building a project
with Bazel on my eight-core machine.
On the x-axis is time in seconds.
The blocks indicate a target being built,
every track represents one CPU core.
Highlighted blocks are on the critical path.
The top eight tracks show a naive build with coarse targets.
I repeatedly analysed this graph,
and broke up the targets on the critical path into smaller targets.
The bottom eight tracks show the final result:
a build that is almost 30 seconds faster
despite doing the same amount of work.

The importance of fine-grained targets is a lesson I learned from Bazel.
Fine-grained targets are the reason that Bazel can build large dependency graphs quickly,
given enough cores.
The similar build tool Buck
[cites][buckft] its ability to transform a coarse-grained graph of build targets
into a more fine-grained action graph as one of the reasons for its speed.

**Evaluate build target definitions lazily.**<br>
Lazy evaluation enables good performance even in large repositories,
because only the targets that are actually needed for a build are evaluated.
The majority of build target definitions does not even need to be parsed.

Lazy evaluation of build definitions is a lesson I learned from [Nix][nix].
It is what makes installing a package from [Nixpkgs][nixpkg] fast.
Even though Nixpkgs is an expression that evaluates to
a dictionary of thousands of interdependent packages (build targets),
installing a single package
reads very few package definitions from disk,
and only the necessary parts are evaluated.
[Guix][guix] is an alternative to Nix that uses Scheme to define packages,
rather than Nix’ custom language.
After pulling a new version of GuixSD (the Guix equivalent of Nixpkgs),
Guix spends several minutes compiling package definitions.
In Nix evaluation feels instant.

Bazel uses lazy target definitions too,
by having many `BUILD` files,
and aligning dependency paths with filesystem paths.
Build files of targets that are not depended upon do not need to be loaded.

Toolchains and dependencies
---------------------------

**The build tool should manage the runtime or compiler toolchain.**<br>
When a toolchain or other dependency needs to be obtained externally,
building devolves from a single-step command
into hours of dependency hunting and configuration fiddling.
Language package managers make it easy to obtain language dependencies,
but they often stop right there.
When a readme informally specifies the toolchain,
rather than a machine-enforceable build definition,
reproducibility suffers.

The build tool that made me realise the importance
of a build tool-managed compiler was [Stack][stack],
a build tool for Haskell.
Managing the compiler means
that I can check out a two-year old commit of [my blog generator][src],
and `stack build` still produces a binary.
In contrast,
I had to reinstall the Python dependencies of my blog while writing this post,
as the artefacts in my virtualenv suddenly became unusable
after a system update had replaced Python 3.6 with 3.7.
My first reinstallation attempt failed,
because I had `CC` set to Clang,
and a Python package tried to build native code that only compiled with GCC.
I am confident that two years from now
I will still be able to build the generator for this blog,
but I am not sure whether I will be able
to get the currently pinned versions of
the Python dependencies to run.

To add another example:
I recently tried to compile a two-year old [Rust project][convec] of mine,
that compiled with a nightly toolchain at the time.
I never wrote down the exact compiler version I used,
so it took me an hour
of trying nightly toolchains that were published around that time,
before I found one that could compile the project.
Fortunately Rust’s version manager recently
[introduced][rustup] a toolchain file,
so the compiler version can now be pinned
as part of the build definition
that is under source control.

**A truly reproducible build requires a controlled build environment.**<br>
Pinning dependencies managed through a language package manager
is a great first step towards reproducibility,
and pinning the toolchain helps too.
However, as long as there are implicit dependencies on the build environment
(such as libraries or tools installed through a system package manager),
*works on my machine* issues persist.

There are two ways to create a controlled build environment:

* Track down *all* implicit dependencies and make them explicit.
  Building inside a sandboxed environment
  can help identify such dependencies
  by making undeclared dependencies unavailable.
  For example,
  if a build step does not specify a dependency on GCC,
  there will be no `gcc` on the `PATH`.
  Nix is an implementation of this approach.
* Admit defeat on tracking dependencies,
  and try and fix the entire environment instead,
  for example by building inside a specific container or virtual machine.
  Care must be taken to avoid
  mutating the environment in uncontrollable ways.
  For example,
  running `apt update` would put an initially pinned
  file system in an indeterminate state again.

Not only language packages
and toolchain,
also native dependencies.
Gets you Nix.
Not always feasible,
is a trade off.
Alternatively build inside VM.

Managing the compiler also removes
much of the need for a traditional `configure` script.
Libraries may want to support multiple toolchains,
but for binaries a lot of complexity can be avoided.

Nix is the only tool I have used that just works.


Ergonomics
----------

**Startup time matters.**<br>
The overhead of interpreters or just in time compilers can be significant.

My experience with Bazel is that although it builds large projects quickly,
it is slow to start.
The build tool runs on the JVM,
and can sometimes take seconds to do a no-op build even in a small repository.
*Please*
— a very similar build system implemented in Go —
is much snappier.
Build definitions that can be evaluated efficiently matter too:
even though both Make and Ninja are native binaries,
Ninja starts building faster.
Ninja traded flexibility in the build file format for faster builds,
deferring complex decisions to a meta build system
such as CMake, Meson, or GN.

Another telling example is the Mercurial source control system.
Its `hg` command is written in Python for extensibility.
This comes at the cost of responsiveness:
just evaluating imports
can already take a significant percentage of total execution time,
which is why parts of Mercurial are now
[being replaced][hgoxid] with native binaries.

References and further reading
------------------------------

Further reading and other related content:

 * [Build Systems à la Carte][carte]
   by Andrey Mokhov, Neil Mitchell, and Simon Peyton Jones,
   classifies build tools based on their rebuilding strategy and scheduling algorithm.
 * [Build Tools as Pure Functional Programs][bpfunc],
   gets to the heart of what a build tool is,
   and draws analogies with functional programming,
   in particular between higher-order functions
   and building toolchains as part of the build.
 * [Compilers are Databases][dotty],
   a talk by Martin Odersky,
   explains how functional reactive programming can be applied to compilers.
 * [Houyhnhnm Computing Chapter 9: Build Systems][ngnghm],
   part of an insightful series that reevaluates computing from a hypothetical alien perspective,
   argues that functional reactive programming is a good fit for build systems.
 * [The Purely Functional Software Deployment Model][thesis],
   Eelco Dolstra’s doctoral thesis,
   introduces Nix and its immutable input-addressable store
   as a basis for building and deploying software.

Tools mentioned throughout this post:

 * The [Bazel][bazel] build system, the open source version of Google’s Blaze
 * The [Buck][buck] build system, inspired by Blaze
 * The [CMake][cmake] meta build system
 * The [GN][gn] meta build system, used in Chromium
 * The [GYP][gyp] meta build system, the predecessor to GN
 * The [Guix][guix] system package manager, inspired by Nix
 * The [Meson][meson] meta build system
 * The [Ninja][ninja] low-level build system, targeted by GN and Meson
 * The [Nix][nix] system package manager
 * The [Pants][pants] build system, inspired by Blaze
 * The [Please][please] build system, inspired by Blaze
 * The [SCons][scons] build system toolkit
 * The [Shake][shake] build system
 * The [Stack][stack] build tool and language package manager
 * The [Waf][waf] build system. TODO: Not actually mentioned

[bazel]:  https://bazel.build/
[bpfunc]: https://www.lihaoyi.com/post/BuildToolsasPureFunctionalPrograms.html
[buck]:   https://buckbuild.com/
[buckft]: https://buckbuild.com/concept/what_makes_buck_so_fast.html
[carte]:  https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/
[cmake]:  https://cmake.org/
[convec]: https://github.com/ruuda/convector
[dotty]:  https://www.youtube.com/watch?v=WxyyJyB_Ssc
[gn]:     https://gn.googlesource.com/gn/
[goma]:   https://chromium.googlesource.com/infra/goma/client
[guix]:   https://www.gnu.org/software/guix/
[gyp]:    https://gyp.gsrc.io/
[hgoxid]: https://www.mercurial-scm.org/wiki/OxidationPlan
[hickey]: https://github.com/tallesl/Rich-Hickey-fanclub
[jussi]:  https://nibblestew.blogspot.com/2018/02/on-unoptimalities-of-language-specific.html
[lngsrv]: https://microsoft.github.io/language-server-protocol/
[meson]:  https://mesonbuild.com/
[ngnghm]: https://ngnghm.github.io/blog/2016/04/26/chapter-9-build-systems/
[ninja]:  https://ninja-build.org/
[nix]:    https://nixos.org/nix/
[nixpkg]: https://nixos.org/nixpkgs/
[pants]:  https://www.pantsbuild.org/
[please]: https://please.build/
[repro]:  https://reproducible-builds.org/
[rustc2]: https://github.com/nikomatsakis/rustc-on-demand-incremental-design-doc/blob/e08b00408bb1ee912642be4c5f78704efd0eedc5/0000-rustc-on-demand-and-incremental.md
[rustc]:  https://blog.rust-lang.org/2016/09/08/incremental.html
[rustup]: https://github.com/rust-lang-nursery/rustup.rs/commit/107d8e5f1ab83ce13cb33a7b4ca0f58198285ee8
[scons]:  https://scons.org/
[sconsc]: https://scons.org/doc/3.0.1/HTML/scons-user/ch24.html
[shake]:  https://shakebuild.com/
[src]:    https://github.com/ruuda/blog
[stack]:  https://haskellstack.org/
[thesis]: https://nixos.org/~eelco/pubs/phd-thesis.pdf
[waf]:    https://waf.io/
