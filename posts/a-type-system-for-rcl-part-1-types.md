---
title: A type system for RCL, part 1: Types
header: A type system for RCL
subheader: Types
part: 1
lang: en-US
date: 2024-04-29
synopsis: TODO
---

<span class="run-in">I am [building][rcl-intro]</span> a new configuration language:
[RCL][rcl-lang].
From the start I intended it to have types,
but initially it was implemented as a completely dynamic language.
Now that I completed the initial version of a typechecker,
I thought it would be interesting to look at the type system and its implementation.
The type system is by no means done,
in particular record types
and importing files across modules are not yet done,
but I think there is enough here for a post or two.
In part one we’ll explore the type system itself,
and in part two we’ll dive into the internals of the typechecker.

My goal with this series is twofold:

 * **Sharing ideas and experiences.**
   None of the ideas in RCL’s type system are particularly innovative or special,
   but the way in which they are put together can still be interesting.
 * **I’m looking for feedback.**
   I feel pretty confident that the way the RCL interpreter is implemented makes sense,
   but I’m less sure about the type system.
   I studied type and effect systems and program analysis in university,
   so none of this is completely new to this,
   but I have a feeling that for some of the choices I made,
   an expert more versed in the literature will say
   “Ah this is a well-known technique with this name”,
   or “this approach doesn’t work, it has that problem”.

[rcl-intro]: /2024/a-reasonable-configuration-language
[rcl-lang]:  https://rcl-lang.org/
