---
title: Alignment starter pack
date: 2024-06-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Last week I was talking
---

Last week I was talking with people about AI alignment.
Despite the amount of attention that machine learning is receiving nowadays,
alignment is still a niche topic.
This worries me,
because a misaligned superintelligence has the potential
to pose an existential threat to humanity,
and I think we should treat it as seriously as nuclear warfare or climate change.
In this post I want to give a few pointers
for where to learn more about AI alignment.

## What is AI alignment?

**Alignment is the process of building an AI
whose goal aligns with the goals of its creators.**
As of 2024, alignment is an unsolved problem.
We have techniques for building <abbr>AI</abbr> whose _output_
matches what its human creators want to see on a range of inputs,
but due to reasons highlighted below,
this does not imply that its _goal_ matches that of its creators.

Recommended resource:

* [A<!---->I Alignment: Why It’s Hard, and Where to Start][stanford-talk]
  by Eliezer Yudkowsky.
  (Also available [in article form][where-to-start].)
  Keep in mind, this talk is from 2016.
  It predates the seminal 2017 paper [_Attention is All You Need_][attention]
  that enabled the current wave of <abbr>LLM</abbr>s.
  The talk is still very relevant,
  and with this context it’s also a great illustration
  of how quickly AI capabilities are evolving.

[stanford-talk]:  https://www.youtube.com/watch?v=EUjc1WuyPT8
[where-to-start]: https://intelligence.org/2016/12/28/ai-alignment-why-its-hard-and-where-to-start/
[attention]:      https://arxiv.org/abs/1706.03762

## Safety vs. alignment

A non-superintelligent AI that is unaligned may be offensive or even harmful,
but it is not an existential threat.
Current efforts around _AI safety_ are focused on
preventing an LLM from saying naughty words,
expressing politically inconvenient statements,
or helping people to build weapons.
Solving alignment would enable us to solve these issues,
but to my knowledge,
current attempts are reactive rather than proactive.
We can mitigate bad behaviors that we are aware of,
but this is not fundamental progress on alignment.
As an analogy,
we can fix buffer overflows in a program after we learn about them,
but that’s not the same as writing the program in a memory-safe language
that eliminates buffer overflows by construction.

## Why misalignment is dangerous

An superintelligence by definition is vastly better than humans at achieving its goal.
If that goal does not include a component that cares about preserving humanity,
when humans and the superintelligence compete for resources,
humans will not stand a chance.
A rogue AI does not even have to turn against humans explicitly
— it simply may not care.
People who worry about alignment worry
that the _default_ state of an AI is to be unaligned for reasons highlighted
below.

Some keywords and recommended resources:

 * [Paperclip maximizer][paperclip] —
   A paperclip maximizer is an agent
   tasked with producing as many paperclips as possible.
   It’s a thought experiment to show that
   a goal that is seemingly harmless in a weak AI
   can become an existential threat in a strong AI.
 * [It Looks Like You’re Trying To Take Over The World][clippy] by Gwern.
   This is a story about how a hard take-off may unfold badly.
   The work is fiction,
   but inspired by existing research.
   It’s worth following some of the links in the story.

[paperclip]: https://www.lesswrong.com/tag/squiggle-maximizer-formerly-paperclip-maximizer
[clippy]:    https://gwern.net/fiction/clippy

## Hard vs. soft take-off

Also called fast and slow.

## Corrigibility

XX
