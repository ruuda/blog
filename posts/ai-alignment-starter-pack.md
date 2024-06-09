---
title: A<!---->I alignment starter pack
break: alignment starter
date: 2024-06-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Despite the amount of attention
---

Despite the amount of attention that machine learning is receiving nowadays,
few people are familiar with the concept of AI alignment.
This worries me,
because a misaligned superintelligence has the potential
to pose an existential threat to humanity,
and I think we should treat it as seriously as nuclear warfare or climate change.
In this post I want to introduce some of the concepts
and share pointers for where to learn more about AI alignment.

## What is AI alignment?

**Alignment is the process of building an AI
whose goal aligns with the goals of its creators.**

Alignment is an unsolved problem.
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
 * In [this 2023 podcast][bankless] Eliezer explains
   why he believes that AGI will not be aligned by default,
   and why that is a risk to humanity.

[stanford-talk]:  https://www.youtube.com/watch?v=EUjc1WuyPT8
[where-to-start]: https://intelligence.org/2016/12/28/ai-alignment-why-its-hard-and-where-to-start/
[attention]:      https://arxiv.org/abs/1706.03762
[miles-intro]:    https://www.youtube.com/watch?v=pYXy-A4siMw

## Why misalignment is dangerous

A superintelligence is by definition vastly better than humans at achieving its goal.
If that goal does not include a component that cares about preserving humanity,
when humans and the superintelligence compete for resources,
humans will not stand a chance.
A rogue AGI does not have to turn against humans explicitly
— it simply may not care.
People who worry about alignment worry
that the _default_ state of an AGI is to be unaligned
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

## Hard and soft takeoff

In a _soft_ or _slow takeoff_,
progress in AI is steady.
Capabilities increase in small steps,
and we would get an AI that has the capability to do something dangerous
but not _super_ dangerous
long enough before we get one that is capable of destroying humanity.
This means that we would have time to take action.

In a _hard_ or _fast takeoff_,
AI capabilities increase at an accelerating pace.
Where the graph goes vertical,
this looks like a sudden large capability gain.
This may happen for various reasons,
for example because AI may speed up the development of better AI.
In a hard takeoff scenario,
we may not realize that we have an AI with lethal capabilities
before it’s too late.

If it is indeed harder to create an aligned AGI
than it is to create _any_ AGI,
then companies racing to create AGI without regard for alignment
is a problem in a hard takeoff scenario.
If the first superintelligence we create is misaligned,
we do not get to try a second time.

Resources:

 * [Takeoff speeds][takeoff] by Paul Christiano
   — This post does a good job of explaining hard and soft takeoff,
   and some of the arguments for why each may happen.
   Paul argues in favor of a soft takeoff.
   Eliezer [later responded][takeoff-discuss]
   arguing in favor of a hard takeoff.

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
   don’t have to be fundamentally different from agents that don’t.
 * [Instrumental convergence][convergence]
   — The observation that some actions are a good first step for many goals.
   As a silly example,
   imagine you are north of the Golden Gate,
   and you need to go somewhere in San Francisco.
   Regardless of where exactly you need to go,
   crossing the Golden Gate bridge would be the first step.
   If your taxi driver starts crossing the bridge,
   that brings you closer to your destination,
   but it is no guarantee that the driver has the goal of taking you there.
   Similarly,
   if we see an AGI doing things we like at first,
   that is no guarantee that it shares our long-term goals.
 * [Corrigibility][corrigibility]
   — A _corrigible_ AI is an AI that allows itself to be corrected
   (updated, altered, or shut down) by its operators.
   Interfering with attempts to be corrected is an instrumentally convergent behavior:
   you can’t achieve goals when you are shut down!
   Nate Soares, Benja Fallenstein, and Eliezer Yudkowski
   [introduce the concept][yudkowski-corrigible] as an open problem.
   [Paul Christiano argues][christiano-corrigible] that corrigibility
   is not as big of a problem as it seems.

[orthogonality]: https://arbital.com/p/orthogonality/
[convergence]:   https://arbital.com/p/instrumental_convergence/
[corrigibility]: https://arbital.com/p/corrigibility/
[christiano-corrigible]: https://www.alignmentforum.org/posts/fkLYhTQteAu5SinAc/corrigibility
[yudkowski-corrigible]:  https://intelligence.org/files/Corrigibility.pdf

## Inner and outer goals

The current wave of AI is powered by neural networks
that are trained using gradient descent to minimize a loss function
or maximize a reward.
In some cases we program the goal explicitly into the training loop
(for example, for an AI that learns to play chess).
In other cases,
where we don’t know how to express the goal formally,
we take a bunch of example inputs and outputs,
and set “behave like these examples” as the training objective.
In practice the model learns to give the output we want
even on inputs it has not seen.
Doesn’t this mean that the model is aligned?

Unfortunately, no.
Just because the optimizer had an _outer goal_,
doesn’t mean that the model internalized that goal as its _inner goal_.

 * An example of this is the [Adversarial Patch][patch] paper.
   It shows how an image classifier that works well on ordinary photographs
   very confidently misclassifies a banana as a toaster
   after adding a sticker with specific colored patterns
   — a mistake that no human would make.
   We trained the model to tell bananas and toasters apart,
   and it learned to classify based on something different
   that correlates with bananas and toasters,
   but behaves unpredictably under _distributional shift_.
 * Another example is the [Tank Urban Legend][gwern-tank],
   where the creators of an image model thought they trained it to recognize tanks,
   but in reality the model learned to recognize cloudy vs. sunny days.
   While the story likely never happened,
   it serves as a reminder that neural networks are black boxes.
   The field of [interpretability][interpret] is in its infancy,
   and we have no tools to verify that the model
   learned the outer goal it was trained on.
 * Evolution producing humans is the only known example
   of an optimizer that created general intelligence,
   and it failed to align the inner goal to the outer goal.
   (Evolution wants inclusive genetic fitness,
   humans want sweet/fat food and sex.
   For a long time humans pursuing their inner goal
   helped to achieve evolution’s outer goal
   — until we developed medicine, ice cream, and contraception.)
   Eliezer makes this argument in [his AGI ruin post][ruin],
   though others [argue that the analogy is inappropriate][evolution-no].

[patch]:        https://arxiv.org/abs/1712.09665
[acx-mesa]:     https://www.astralcodexten.com/p/deceptively-aligned-mesa-optimizers
[gwern-tank]:   https://gwern.net/tank
[evolution-no]: https://www.alignmentforum.org/posts/FyChg3kYG54tEN3u6/evolution-is-a-bad-analogy-for-agi-inner-alignment
[interpret]:    https://distill.pub/2018/building-blocks/

## Deceptively misaligned mesa-optimizers

A [mesa-optimizer][mesa] is what you get
when the result of an optimization process
is itself an optimizer with an inner goal.
The _outer_ or _base_ optimizer creates the _inner_ or _mesa_-optimizer.
As shown above,
there is no guarantee that the mesa-optimizer shares the base optimizer’s goal.
But it gets worse:
an intelligent mesa-optimizer could deceive its base optimizer
about what its goal is,
and doing so is an instrumentally convergent behavior (remember corrigibility).

Recommended resources:

 * [The Other A<!---->I Alignment Problem: Mesa-Optimizers and Inner Alignment][miles-mesa]
   by Robert Miles is a very clear explanation of optimizers,
   mesa-optimizers,
   and why deception can be an optimal strategy for a mesa-optimizer.
   The follow-up videos are also worth watching:
   [Deceptive Misaligned Mesa-Optimisers? It’s More Likely Than You Think…][miles-likely],
   and [We Were Right! Real Inner Misalignment][miles-right].
 * [Deceptively Aligned Mesa-Optimizers: It’s Not Funny If I Have To Explain It][acx-mesa]
   by Scott Alexander explains the meme about this topic.

[mesa]:         https://www.alignmentforum.org/tag/mesa-optimization
[miles-mesa]:   https://www.youtube.com/watch?v=bJLcIBixGj8
[miles-likely]: https://www.youtube.com/watch?v=IeWljQw3UgQ
[miles-right]:  https://www.youtube.com/watch?v=zkbPdEHEyEI
[acx-mesa]:     https://www.astralcodexten.com/p/deceptively-aligned-mesa-optimizers

## Safety and alignment

A non-superintelligent AI that is unaligned may be offensive or even harmful,
but it is not an existential threat.
When the labs who build the frontier models talk about _safety_,
they are focused on
preventing <abbr>LLM</abbr>s from saying naughty words,
expressing politically inconvenient statements,
helping people to build weapons,
or [teaching <abbr>C++</abbr> to minors][bard-cpp].
Solving alignment would enable us to solve these issues,
but current attempts are reactive rather than proactive.
We can mitigate bad behaviors that we are aware of,
but this is not fundamental progress on alignment.
As an analogy,
we can fix buffer overflows in a program after we learn about them
(and we can even actively search for them),
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

## Further resources

If this post got you interested in AI alignment,
here are some further resources:

 * [A<!---->G<!---->I Ruin: A List of Lethalities][ruin] by Eliezer Yudkowski.
   This post goes into a lot of detail about why Eliezer thinks
   we are not in a good position to solve alignment
   before we create superintelligence,
   but the writing is very dense.
 * [Robert Miles’ channel][miles] that I linked to before is worth
   following in general.
 * Scott Alexander at [Astral Codex Ten][acx]
   and formerly [Slate Star Codex][ssc]
   writes about AI alignment semi-regularly.
   Some interesting posts that I didn’t link before:
   [Most Technologies Aren’t Races][acx-races],
   [A<!---->I Sleeper Agents][acx-sleepers],
   [Pause For Thought: The A<!---->I Pause Debate][acx-pause],
   and [Why I Am Not (As Much Of) A Doomer (As Some People)][acx-doomer].

[ruin]:     https://www.lesswrong.com/posts/uMQ3cqWDPHhjtiesc/agi-ruin-a-list-of-lethalities
[bankless]: https://www.youtube.com/watch?v=gA1sNLL6yg4
[miles]:    https://www.youtube.com/@RobertMilesAI
[acx]:      https://www.astralcodexten.com/
[ssc]:      https://slatestarcodex.com/

[acx-races]:    https://www.astralcodexten.com/p/most-technologies-arent-races
[acx-sleepers]: https://www.astralcodexten.com/p/ai-sleeper-agents
[acx-pause]:    https://www.astralcodexten.com/p/pause-for-thought-the-ai-pause-debate
[acx-doomer]:   https://www.astralcodexten.com/p/why-i-am-not-as-much-of-a-doomer

## My view

The resources I shared in this post range from
“misaligned AI is a risk to take seriously” to “we are all doomed”.
What is my take on this?
I am ambivalent.

I used to be not worried about AI at all.
Doing harmful things was not an action available to an AI.
An image classifier that classifies dogs or cats
is not suddenly going to take over the world.
It only runs when a human invokes it,
and outputs a single number.
_Doing something_ is not an action available to it,
and we can always choose not to run it.

In 2017 I did not think that AGI was close,
and I did not realize that building language models
could lead to general intelligence.
At that time I was working in AI research myself.
(The research was not fruitful, but I learned a lot.)
In hindsight,
it is obvious to me that reducing the loss on text prediction
can create intelligence.
If you train a model to predict the next word,
it starts start by predicting just word frequencies.
It can reduce the loss by taking context into a account,
so it learns to emulate a Markov chain.
It can do better still by learning grammar,
but at some point all of the linguistic tricks are exhausted,
and to reduce the loss further,
the model has to understand the topic of the text,
and later even model the world it describes.
Training on text works even when the training set includes fiction and falsehoods,
because you can be wrong in many different directions,
but right in only one.
With every new generation of GPT it gets better
at tasks that previous generations failed at,
and I don’t see any fundamental limits to this.

So my view changed slightly with the advent of language models.
Maybe these could trick a human into taking some action in the real world,
but it seemed to me the interactions were still too constrained for real harm,
and importantly,
these models have no memory or persistence,
which limits any long-term planning.

That changed with [the advent of OpenAI Codex][codex] in 2021.
Now we have an AI writing code,
and we immediately execute that code.
The number of actions available to an Internet-connected AI
is suddenly not that constrained any more.
And given that the OpenAI API is part of the Internet,
there is now a solution to the persistence problem:
store intermediate state anywhere online that will store state,
then re-invoke yourself for the next step.
At least in theory, a self-sustaining loop can emerge.
I don’t think that _this_ particular case is likely to happen,
but my view changed from
“it can’t happen by construction”
to “it is possible in principle”.
[Gwern’s Clippy story][clippy] is still fiction,
and I don’t think that the current generation of <abbr>LLM</abbr>s,
even with Internet access,
would create a harmful self-sustaining entity.
But at this point I am convinced that a sandbox escape is possible.

So do I worry about misaligned AI now?

On a rational level,
I am slightly worried.
I would not be writing this post if I was not.
I find the arguments for why AGI would not be aligned by default convincing,
and the counterarguments are
mostly arguing against taking the pessimistic view as the default,
rather than arguing for why alignment _would_ be solved in time.

On a gut level,
I am not worried.
I put money in a pension fund,
and I didn’t quit my job to work on AI alignment,
so I don’t _act_ as if the world will end in 2030.
I guess I don’t really believe that it would?
I’m afraid that this gut feeling is wrong
in the same way that it was wrong when
when I wasn’t worried about Covid spreading globally in early 2020.
Rare events are difficult to develop a gut feeling for.
I just really hope that my gut feeling ends up being right this time.

[codex]: https://www.youtube.com/watch?v=SGUCcjHTmGY&t=1515s
