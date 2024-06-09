---
title: Alignment starter pack
date: 2024-06-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Despite the amount of attention
---

Despite the amount of attention that machine learning is receiving nowadays,
alignment is still a niche topic.
This worries me,
because a misaligned superintelligence has the potential
to pose an existential threat to humanity,
and I think we should treat it as seriously as nuclear warfare or climate change.
In this post I want to mention some of the keywords
and share pointers for where to learn more about AI alignment.

## What is AI alignment?

**Alignment is the process of building an AI
whose goal aligns with the goals of its creators.**

As of 2024, alignment is an unsolved problem.
We have techniques for building <abbr>AI</abbr> whose _output_
matches what its human creators want to see on a range of inputs,
but due to reasons highlighted below,
this does not imply that its _goal_ matches that of its creators.

Recommended resources:

 * [Intro to A<!---->I Safety][miles-intro] by Robert Miles
   — A great general introduction about why AGI might be dangerous,
   and to the concepts that I mention in the remainder of this post.
 * [A<!---->I Alignment: Why It’s Hard, and Where to Start][where-to-start]
   by Eliezer Yudkowsky ([video][stanford-talk]).
   This is good introduction with more details about alignment specifically.
   The talk is from 2016;
   it predates the seminal 2017 paper [_Attention is All You Need_][attention]
   that enabled the current wave of <abbr>LLM</abbr>s.
   The talk is still very relevant,
   and 8 years later
   it’s a great illustration of how quickly AI capabilities are evolving.

[stanford-talk]:  https://www.youtube.com/watch?v=EUjc1WuyPT8
[where-to-start]: https://intelligence.org/2016/12/28/ai-alignment-why-its-hard-and-where-to-start/
[attention]:      https://arxiv.org/abs/1706.03762
[miles-intro]:    https://www.youtube.com/watch?v=pYXy-A4siMw

## Why misalignment is dangerous

A superintelligence is by definition vastly better than humans at achieving its goal.
If that goal does not include a component that cares about preserving humanity,
when humans and the superintelligence compete for resources,
humans will not stand a chance.
A rogue AI does not have to turn against humans explicitly
— it simply may not care.
People who worry about alignment worry
that the _default_ state of an AI is to be unaligned
for reasons highlighted in the next sections.

Recommended resources:

 * [Paperclip maximizer][paperclip] —
   A paperclip maximizer is an agent
   tasked with producing as many paperclips as possible.
   It’s a thought experiment to show that
   a goal that is seemingly harmless in a weak AI
   can become an existential threat in a strong AI.
 * [It Looks Like You’re Trying To Take Over The World][clippy] by Gwern
   — A story about how a hard takeoff may unfold badly.
   The work is fiction,
   but inspired by existing research.
   In typical Gwern style it’s full of hyperlinks
   to more details behind the events in the story.

[paperclip]: https://www.lesswrong.com/tag/squiggle-maximizer-formerly-paperclip-maximizer
[clippy]:    https://gwern.net/fiction/clippy

## Hard vs. soft takeoff

In a _soft_ or _slow takeoff_,
progress in AI is steady.
Capabilities increase in small steps,
and we would get an AI that has the capability to do something dangerous
but not _super_ dangerous
long enough before we get one that is capable of destroying humanity.
This means that we would have time to take action.

In a _hard_ or _fast takeoff_,
AI capabilities increase suddenly by a large amount.
This may happen for various reasons,
for example because AI may speed up the development of better AI.
In a hard takeoff scenario,
we may not realize that we have an AI with lethal capabilities
before it’s too late.

Resources:

 * [Takeoff speeds][takeoff] by Paul Christiano
   — This post does a good job of explaining hard and soft takeoff,
   and some of the arguments for why each may happen.
   Paul argues in favor of a soft takeoff.
   Eliezer [later discussed this post][takeoff-discuss]
   where he argues in favor of a hard takeoff.

[takeoff]:         https://sideways-view.com/2018/02/24/takeoff-speeds/
[takeoff-discuss]: https://www.alignmentforum.org/posts/vwLxd6hhFvPbvKmBH/yudkowsky-and-christiano-discuss-takeoff-speeds

## Orthogonality, instrumental convergence, and corrigibility

These topics come up often
in discussions about why a misaligned AI might exist and even be likely,
and why we may not be able to tell that it’s misaligned.

 * [Orthogonality][orthogonality]
   — The Orthogonality Thesis states that there can exist intelligent agents
   that can pursue any kind of goal (such as maximizing paperclips),
   and that agents that have a goal that seems absurd to humans
   don’t have to be fundamentally different.
 * [Instrumental convergence][convergence]
   — The observation that some actions are a good first step for many goals.
   As a silly example,
   imagine you are north of the Golden Gate,
   and you need to go somewhere in San Francisco.
   Regardless of where exactly you need to go,
   crossing the Golden Gate bridge would be the first step.
   If your taxi driver starts crossing the bridge,
   that brings you closer to your destination at first,
   but it is no guarantee that the driver has the goal of taking you there.
   Similarly,
   if we see an AGI doing things we like at first,
   that is no guarantee that it shares our goals.
 * [Corrigibility][corrigibility]
   — A _corrigible_ AI is an AI that allows itself to be corrected
   (updated, altered, or shut down) by its operators.
   Interfering with attempts to be corrected is an instrumentally convergent behavior:
   you can’t achieve goals when you are shut down!
   In [this post][christiano-corrigible],
   Paul Christiano responds to the [_Corrigibility_][yudkowski-corrigible] paper
   by Soares, Fallenstein, and Yudkowski.

[orthogonality]: https://arbital.com/p/orthogonality/
[convergence]:   https://arbital.com/p/instrumental_convergence/
[corrigibility]: https://arbital.com/p/corrigibility/
[christiano-corrigible]: https://www.alignmentforum.org/posts/fkLYhTQteAu5SinAc/corrigibility
[yudkowski-corrigible]:  https://intelligence.org/files/Corrigibility.pdf

## Deceptively misaligned mesa-optimizers

TODO

## “Safety” vs. alignment

A non-superintelligent AI that is unaligned may be offensive or even harmful,
but it is not an existential threat.
When the labs who build the frontier models talk about _safety_,
they are focused on
preventing <abbr>LLM</abbr>s from saying naughty words,
expressing politically inconvenient statements,
helping people to build weapons,
or [teaching <abbr>C++</abbr> to minors][bard-cpp].
Solving alignment would enable us to solve these issues,
but to my knowledge,
current attempts are reactive rather than proactive.
We can mitigate bad behaviors that we are aware of,
but this is not fundamental progress on alignment.
As an analogy,
we can fix buffer overflows in a program after we learn about them,
but that’s not the same as writing the program in a memory-safe language
that eliminates buffer overflows by construction.

Resources:

 * [Perhaps It Is A Bad Thing
    That The World's Leading A<!---->I Companies
    Cannot Control Their A<!---->Is][acx-control]
   by Scott Alexander
   — An opinion on why addressing the superficial safety issues
   in the short term may be harmful for serious alignment attempts
   in the long run.

[bard-cpp]: https://www.reddit.com/media?url=https%3A%2F%2Fi.redd.it%2Fag8rhv9n5dmc1.png
[acx-control]: https://www.astralcodexten.com/p/perhaps-it-is-a-bad-thing-that-the

## Grim outlook

 * [A<!---->G<!---->I Ruin: A List of Lethalities][ruin] by Eliezer Yudkowski.
   This post goes into a _lot_ of detail of

[ruin]: https://www.lesswrong.com/posts/uMQ3cqWDPHhjtiesc/agi-ruin-a-list-of-lethalities
