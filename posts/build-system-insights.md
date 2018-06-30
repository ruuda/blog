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
   After some time a build artifact should appear.
 * The build artifact does not depend on where or when I perform the build,
   only on the source code checked out
   and flags passed to the build command (such as `--debug`, `--profile` or `-c opt`).
 * When I check out a three-year old commit,
   all of the above should still be true,
   and I should get the same build artifacts that I got three years ago.
 * It should produce the artifact quickly.

Build definition insights
-------------------------

**Build target definitions should live as close to the source code as possible.**
Unlike a global makefile or other build definition in the repository root,
a distributed approach with definitions scattered throughout the repository
remains maintainable even in very large repositories. 

This is a lesson I learned from Bazel and GN. Pants, being inspired by Blaze
(Google’s internal version of Bazel) also applies the principle.

**Build target definitions should be evaluated lazily.**
Lazy evaluation enables good performance even in large repositories,
because only the targets that are actually needed for a build are evaluated.
The majority of build target definitions does not even need to be parsed.

Lazy evaluation of build definitions is a lesson I learned from Nix.
It is what makes installing a package from Nixpkgs fast.
Even though Nixpkgs is essentially an expression that evaluates
to a dictionary of thousands of interdependent packages (build targets),
installing a single package reads very few package definitions from disk.
Guix is an alternative to Nix that uses Scheme as its base language,
rather than Nix’ custom language.
After pulling a new version of GuixSD (the equivalent of Nixpkgs),
Guix spends several minutes compiling package definitions.
In Nix evaluation feels instant.

Bazel applies this principle too by having many `BUILD` files,
and aligning dependency paths with filesystem paths.
Build files of targets that are not depended upon do not need to be loaded.

References
----------

 * The [Bazel][bazel] build system, the open source version of Google’s Blaze
 * The [GN][gn] meta-build system
 * The [Meson][meson] build system
 * The [Ninja][ninja] low-level build system, a target for GN and Meson
 * The [Nix][nix] system package manager
 * The [Pants][pants] build system, inspired by Blaze
 * The [Shake][shake] build system
 * The [Stack][stack] build tool and language package manager

[bazel]:  https://bazel.build/
[gn]:     https://chromium.googlesource.com/chromium/src/+/master/tools/gn/README.md
[hickey]: https://github.com/tallesl/Rich-Hickey-fanclub
[meson]:  https://mesonbuild.com/
[ninja]:  https://ninja-build.org/
[nix]:    https://nixos.org/nix/
[pants]:  https://www.pantsbuild.org/
[repro]:  https://reproducible-builds.org/
[shake]:  https://shakebuild.com/
[stack]:  https://haskellstack.org/
