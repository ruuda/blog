---
title: Contributions, code review, and A<!---->I
date: 2026-02-02
lang: en-US
synopsis: TODO
minutes: ?
run-in: I maintain several
teaser: llm-interactions
---

I maintain several open source projects.
Some of them are somewhat popular,
and attract contributions.
In the age of <abbr>LLM</abbr>s,
I increasingly doubt whether they are worth responding to.

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
But if that was all,
it would be less effort to fix the code myself,
without having to explain _why_ this way is better as well.
My project doesn’t really _need_ this contribution.
I’m happy to accept it,
but I’m also fine without it.
I think the real reason I’m reviewing this is:
somebody spent time and effort to try and help,
and I feel that I owe them a response.
This review is not a case of bikeshedding about preferences,
the code can be made simpler and more efficient at the same time.
The author could learn something general about programming,
and some things about Rust,
not just what the conventions of my project are.
And I’m happy to teach!
I actually enjoy doing that!

But I start to wonder, who _is_ the author of this code?
If it’s a junior programmer,
I would be very happy to review.
To show how the code can be improved,
and help the author grow.
I think that’s a good use of my evening,
and it brings me fulfilment.
But what if the author is an LLM,
and the human behind the account is merely going to forward my comments
to their Claude Code subscription?
Then nobody is learning anything here,
and doing a review would be a complete waste of my time.
And if I still want the example at all,
it would be less effort to just fix it myself,
without also having to explain why it’s better that way.

The frustrating thing is … I can’t tell which situation I’m in!
I am genuinely unsure about the amount of AI I’m dealing with here.
Maybe there was no LLM involved at all,
and all the slop around me is making me paranoid.
Maybe it’s mostly real,
but the person used some LLM autocomplete,
or a few rounds of asking an LLM how to get the code to compile.
I don’t think it’s an AI agent acting entirely autonomously,
but who knows?
I do have an entry in my `CONTRIBUTING.md`
that says LLM-generated code is not allowed,
but would the author have read that?
