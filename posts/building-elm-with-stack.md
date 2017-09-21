---
title: Building Elm with Stack
break: Elm with
date: 2017-09-21
minutes: 4
synopsis: The Haskell tool Stack enables simple reproducible builds. I used it to build the Elm compiler from source.
run-in: The hackathon I joined
---

The hackathon I joined last weekend was a nice opportunity to try the [Elm][elm] language.
Getting the Elm compiler on my system proved harder than expected though,
and I ended up building it from source.
It is a nice excuse for me to share a few thoughts on package management and reproducible builds.
If you just want to build Elm 0.18 from source,
you can skip straight to the [`stack.yaml`](#building-elm-with-stack).

Prelude
-------
My first attempt to get the Elm platform onto my Arch Linux system was not succesful.
I had tried Elm before about a year ago,
and I still had an installation of an older version of the Elm platform on my machine,
courtesey of the [`elm-platform`][elm-platform] package from the Arch User Repository.
The Elm compiler is written in Haskell,
and the `elm-platform` package builds it from source.
A new version of the package was available,
but it failed to build,
because Arch currently ships a newer version of GHC than the one Elm requires to build.
And frankly I did not want to install the GHC and Cabal packages to build it either.
A few months ago their Arch maintainer started packaging all Haskell dependencies as individual packages,
which causes a large amount of churn during updates,
and requires significant additional disk space.
I uninstalled them because I use [Stack][stack] for Haskell development,
which can manage GHC anyway.
It has a fine [`stack-static`][stack-static] package in the AUR.

With no working package in the official Arch repositories or the AUR,
I turned to the Elm website to check out the recommended installation method.
It told me to `npm install` or build from source.
My system already has a package manager that I use to keep programs up to date,
and I refuse to install yet another package manager that I would have to run to perform management tasks.
(Especially if that involves running Javascript outside of a browser.)
The role of a language package manager should be to manage build-time dependencies,
not to distribute user-facing programs to end users.
I decided to build from source.

Elm provides [`BuildFromSource.hs`][fromsource], a script that automates building from source.
It demands GHC 7.10,
so I checked [stackage.org][stackage] to find a Stackage snaphot based on that compiler,
and I ran the script with with the right version of `runghc`:

    stack setup --resolver lts-6.35
    stack runghc --resolver lts-6.35 BuildFromSource.hs 0.18

After commenting out a check that verifies that Cabal is available,
it cloned the Elm repositories.
Then it failed,
because indeed I did not have a `cabal` binary.
Reluctantly I installed Arch’s Cabal package after all,
but this got me no further;
building now failed with a complaint about GHC package paths.
Rather than digging deeper,
I figured it would be easier to make the projects build with Stack.

<h2 id="building-elm-with-stack">Building Elm with Stack</h2>

I added the following `stack.yaml` to the directory that contained the various Elm repositories:

```yaml
resolver: lts-6.35
allow-newer: true
packages:
  - elm-compiler
  - elm-make
  - elm-package
  - elm-reactor
  - elm-repl
```

**This was all that was required, really.**
After a single `stack build`,
the binaries were in `$(stack path --local-install-root)/bin`.
It is a bit embarassing how easy this was,
after all the things I had tried before.

If you want to try this yourself,
there is no need to use Elm’s `BuildFromSource.hs` to clone the repositories.
You can just clone them directly:

```sh
for project in compiler make package reactor repl; do
  git clone https://github.com/elm-lang/elm-$project --branch 0.18.0
done
```

Reproducible builds
-------------------
This adventure with Elm is in my opinion a good example of a bigger issue with reproducible builds,
or builds that are *producible* at all.
If building a given commit produces a binary on one machine,
I want it to produce the same binary on a different machine.
Ideally it should be bitwise identical,
although that is often difficult for unfortunate reasons.
But at the very least the source fed into the compiler should be identical.
And if a given commit compiles today,
I want it to still compile three years from now.
Making a build reproducible in this sense
requires pinning the versions of all dependencies.
And if future compiler releases are not backwards compatible,
the compiler must be pinned as well.
Recording that metadata is one thing,
but obtaining the right compiler can be difficult in practice.
Some languages solve this with another layer of indirection,
by having a version manager manage the package manager and compiler or runtime.
But I think Stack got it right here by making the build tool manage the compiler:
`stack build` just works,
and it does the right thing.

Somewhat ironically,
if I want to compile multiple projects that use different Elm versions,
I will need to be able to switch Elm binaries.
Fortunately I now have a way to obtain the binaries for a given release that is not too painful,
and I can live with having to modify the `PATH` occasionally.
In many ways this is even preferable over having a single system-wide version,
because multiple versions can coexist.

In the end I got Elm working with just enough time left for the hackathon.
More about that soon.

[elm]:          http://elm-lang.org/ <!-- 2017 and not https? D: -->
[elm-platform]: https://aur.archlinux.org/packages/elm-platform/
[stack]:        https://haskellstack.org
[stack-static]: https://aur.archlinux.org/packages/stack-static/
[fromsource]:   https://github.com/elm-lang/elm-platform/blob/c83832cd38091033288a62d8b8ce9f1694454d9a/installers/BuildFromSource.hs
[stackage]:     https://www.stackage.org/
