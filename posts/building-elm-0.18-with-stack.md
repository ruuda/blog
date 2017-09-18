---
title: Building Elm 0.18 with Stack
break: 0.18 with
date: 2017-09-18
minutes: ??
synopsis: ??
run-in: ??
---

I pulled out [Elm][elm] this weekend for a Hackathon.

Prelude
-------
Summary of this paragraph.
The last time I used Elm was about a year ago,
and I still had an installation of the Elm Platform 0.17 on my machine,
courtesey of the [`elm-platform`][elm-platform] package from the AUR.
Unfortunately updating did not do the trick.
Elm does not compile under GHC 8.2.1,
which is what Arch currently ships.
And frankly I did not want to install the GHC and Cabal packages either.
A few months ago their Arch maintainer started packaging all Haskell dependencies as individual packages,
which causes a large amount of churn during updates,
and requires additional disk space.
I uninstalled them because I use Stack for Haskell development,
which can manage GHC anyway.
It has a fine [`stack-static`][stack-static] package in the AUR.

With no working package in the official Arch repositories or the AUR,
I turned to the Elm website to check out the recommended installation instructions.
It told me to `npm install` or build from source.
My system already has a package manager that I use to keep programs up to date,
and I refuse to install yet another package manager that I would have to run to perform updates.
(Especially if that involves running Javascript outside of a browser.)
The role of a language package manager should be to manage build-time dependencies,
not to distribute binaries to end users.
Building from source it was then.

Building Elm from source is a fairly well-documented process,
and [a script][fromsource] that automates it is provided.
It runs on GHC 7.10,
so I checked out [stackage.org][stackage] to find a Stackage snaphot based on that compiler,
and I ran the script with with the right version of `runghc`:

    $ stack setup --resolver lts-6.35
    $ stack runghc --resolver lts-6.35 BuildFromSource.hs 0.18

After commenting out a check that verifies that `cabal` is available,
it cloned the Elm repositories.
Then it failed,
because indeed I did not have a `cabal` binary.
Rather than trying to obtain the right version of Cabal,
I figured I might try to make the packages build with Stack.

Building Elm with Stack
-----------------------

[elm]:          http://elm-lang.org/ <!-- 2017 and not https? D: -->
[elm-platform]: https://aur.archlinux.org/packages/elm-platform/
[stack-static]: https://aur.archlinux.org/packages/stack-static/
[fromsource]:   https://github.com/elm-lang/elm-platform/blob/c83832cd38091033288a62d8b8ce9f1694454d9a/installers/BuildFromSource.hs
[stackage]:     https://www.stackage.org/
