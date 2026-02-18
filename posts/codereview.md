---
title: Contributions, code review, and A<!---->I
date: 2026-02-02
lang: en-US
synopsis: A<!---->I has gotten to a point where I can no longer tell to what extent I’m dealing with a human online, and it’s taking a toll on my mental health.
minutes: ?
run-in: I maintain several
teaser: llm-interactions
---

I maintain several open source projects.
Some of them are somewhat popular,
and attract contributions.
In the age of <abbr>LLM</abbr>s,
I increasingly doubt whether I should continue accepting them.

## An example

A GitHub user opens a pull request against one of my repositories.
It adds an example.
I don’t think it was badly needed,
but I can see that it may be helpful,
and the fact that the author took the time to open a pull request
is strong evidence that they would have found it helpful.
I skim though the code,
it looks valid at first.
I start reading it in more detail,
and I realize that all of this could be far simpler,
so I start explaining how.
I make my way through the diff,
and near the end I notice a pattern
that I often see in LLM-generated code:
a comment that announces what the next few lines do,
even though that’s already very clear from the code.
I start to doubt.
Was this code written by a human or an LLM?
Upon closer inspection,
there are weak signs elsewhere.
Not conclusive evidence — sometimes humans do something
that AI is now infamous for.
I’m about 65% sure there was an LLM involved in some way,
but I don’t know to what extent.

Take a step back, Ruud.
Why am I reviewing this code?
For one, it’s to guard the quality of my project.
But my project doesn’t really _need_ this contribution.
I’m happy to accept it,
but I’m also fine without it.
If quality was all there is to it,
it would be less effort to just fix the code myself,
without also having to explain why that way is better.
I think the real reason I’m reviewing this is:
somebody spent time and effort to try and help,
and I feel that I owe them a response.
My review is not bikeshedding,
the code can be made simpler and more efficient.
The author could learn something general here,
not just what the conventions of my project are.
And I’m happy to teach,
I actually enjoy doing that!

But I start to wonder, who _is_ the author of this code?
If it’s a real programmer,
I would be very happy to review.
To show how the code can be improved,
and help the author grow.
I think that’s a good use of my evening,
and helping others brings me fulfillment.
But what if the author is an LLM,
and the human behind the account is merely going to forward my comments
to their Claude Code subscription?
Then nobody is learning anything here,
and doing a review would be a complete waste of my time.

The frustrating thing is … I can’t tell which situation I’m in!
I am genuinely unsure about how much AI is involved.
Maybe there was no LLM involved at all,
and all the slop around me is making me paranoid.
Maybe it was mostly written by a human,
but the person used LLM autocomplete,
or a few rounds of asking an LLM how to get the code to compile.
I don’t think this case is an AI agent acting entirely autonomously,
but I’ve been in other situations where I suspect that was the case.
I do have a line in my `CONTRIBUTING.md`
that says LLM-generated code is not allowed,
but do people who rely heavily on <abbr>LLM</abbr>s actually read the docs?

## Mostly bad outcomes

What makes this situation so sad
is that any wrong judgment makes me feel bad.
If a real human spent their time and effort trying to help me,
and in return I wrongly accuse them of sending me AI slop,
that would be offensive and I would feel very bad about it.
But if I assume good intent,
I spend my limited free time to try and do a useful review,
and then the account replies with
“You’re absolutely right!”
… then I’d be both furious and feel like a fool at the same time.
Even if it’s an LLM-generated contribution and I correctly notice that,
it still wasted my time,
and I probably disappoint the person who submitted it thinking it was helpful.
It feels bad to disappoint somebody who had good intentions,
even if it’s an anonymous person on the Internet.
The only good outcome here
is when the contribution was fully human after all,
and that seems increasingly rare.

## Now what?

I work on open source because it brings me joy.
If something I built can help somebody,
that’s very fulfilling,
and so is helping others grow.
Receiving a contribution used to be a source of pride:
recognition that something I made was useful,
and even worth enough for somebody to spend time trying to improve it.
All of this is still true of course.
But if every new issue or pull request causes me
to stress about whether AI is wasting my time
or whether I’m offending a human
… is it still worth it?

This feeling is not entirely new.
Already before the rise of <abbr>LLM</abbr>s
I occasionally received well-intentioned but low-quality contributions,
where helping the author get them into a good state
takes me far more effort than I can spend,
but rejecting a well-intentioned contribution also makes me feel bad.
This was a source of maintainer fatigue for me already,
but with the rise of <abbr>LLM</abbr>s it’s taking on new levels.

I don’t know what to do about this.
I added a no-<abbr>LLM</abbr>s rule to the `CONTRIBUTING.md` in my projects
precisely to try and prevent disappointing people who use such tools,
but I still regularly wonder whether I’m dealing with a human or not,
and it’s taking a toll on me.
It’s easy to say that volunteers don’t owe anybody anything,
but being a maintainer of somewhat widely used projects
does come with a feeling of responsibility,
and I don’t want to close the door
for people who genuinely want to improve things.

## It was fine, this time

What happened to the pull request?
I asked the author to confirm that the code was written without AI involvement.
The reply sounded likely human to me,
so I left the review,
the author addressed the comments,
and I merged it.
I fretted about bad outcomes for nothing,
it went as well as it could have!
And yet,
I feel a bit sad that an interaction
that would have been overwhelmingly positive five years ago
<!-- (my project has users! we’re improving it together!), -->
now mostly caused me stress,
through no fault of anybody involved.
