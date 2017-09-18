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
which requires additional disk space and causes a large amount of churn during updates.
I uninstalled them because Stack can manage GHC anyway,
and there is a fine [`stack-static`][stack-static] package in the AUR
without the 700 MB of transitive dependencies.

Building Elm with Stack
-----------------------

[elm]:          http://elm-lang.org/ <!-- 2017 and not https? D: -->
[elm-platform]: https://aur.archlinux.org/packages/elm-platform/
[stack-static]: https://aur.archlinux.org/packages/stack-static/
