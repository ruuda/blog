---
title: A perspective shift on automated market makers through MEV
date: 2021-12-07
minutes: 4
synopsis: A swap at an automated market maker is not like a market order, it behaves more like a limit order. This is the change of perspective that MEV is forcing us to make.
run-in: It takes time
---

It takes time for new ideas to be properly understood.
Often the way we first formulate an idea,
is in hindsight not the simplest or the most elegant way,
and a change of perspective can lead to new understanding.
I think we are in this situation with automated market makers right now,
and MEV ([miner/maximum extractable value][mev]) is forcing a perspective shift.

Automated market makers (<abbr>AMM</abbr>s for short) are a relatively recent development,
and although the math behind swaps is well understood,
I don’t think that this is the full picture.
Even if the swap itself is well understood, the mechanism can be flawed.

[Flash liquidity in Uniswap v3][flashlp] is a clear example of this:
it’s a mechanism that admits a profitable strategy which was not intended by the developers,
and this profit comes at the expense of the intended users (regular liquidity providers).
In a sense, it’s a game-theoretic vulnerability in the protocol.
Sometimes these flaws can be fixed at the protocol level,
but sometimes they indicate a flaw in our understanding.

[mev]:     https://ethereum.org/en/developers/docs/mev/
[flashlp]: https://twitter.com/revertfinance/status/1409642606082940930

Swaps are limit orders, not market orders
-----------------------------------------

My original understanding of AMM swaps was that they are like market orders:
you trade asset X for asset Y,
and you pay whatever the pool’s current price is at the time the swap executes.
When the swap executes, the pool price may be different
from the price at the time when you entered the order,
and to protect against unexpected expenses,
you can enter a maximum slippage percentage.
This sets the maximum price that you are willing to pay for the asset.
If the pool price is above your maximum when the swap executes,
the transaction fails.
I think the intent of the max slippage was just that:
to limit slippage at busy times,
when the price can change due to other swaps executing before yours.

All was well for a while,
until miners discovered _miner extractable value_
(now also called _maximal extractable value_), or MEV for short.
Extractors started [sandwiching][sandwiching] swaps,
and the effect of that is that you always pay your maximum price,
not the pool price.

In a sense,
in the presence of sandwiching,
a swap behaves more like a limit order than a market order:
if it executes at all, it executes at the price that _you_ set,
not at the market price.
Like with a limit order,
the price you pick is a trade-off:
at lower prices your order might not be filled (your transaction may fail),
at higher prices you risk overpaying.

[sandwiching]: https://ethereum.org/en/developers/docs/mev/#mev-examples-sandwich-trading

If you accept sandwiching as a fact of life,
then our understanding of <abbr>AMM</abbr>s is wrong:
the pool price is not the market price,
it’s a minimum price.
We’ve got our user interfaces backwards:
we shouldn’t display the pool price
and then hide the max slippage under advanced settings,
we should own up to it and include the selected slippage in the price.

Make it a feature, not a bug
----------------------------

If you look at it from this point of view,
isn’t it crazy that we let MEV extractors
take the difference between the pool price and the maximum price?
Isn’t that a flaw in the AMM’s design?
A game-theoretic vulnerability?

So here is a proposal: **let the AMM take the difference instead**.
Users who swap always pay their maximum price,
and the difference between the pool price and the maximum price
goes to liquidity providers, as part of the swap fee.
For users, nothing changes.
They pay their maximum price either way.
Liquidity providers benefit,
and the MEV opportunity goes away.

Conclusion
----------

When software contains a critical vulnerability,
we frown upon the attackers who exploit it,
but we also recognize that the only way to address the issue is by patching the software.
What is really bad,
is a software vendor who leaves a vulnerability unpatched
when it is being exploited in the wild.

I think we should approach sandwiching
— and unintended MEV opportunities more
generally — the same way: as a flaw that needs to be patched.
I frown upon the MEV extractors who sandwich.
But at the same time, I think the real blame is with <abbr>AMM</abbr>s
for not doing anything about it.
This is not obvious if you think of AMM swaps as market orders.
But MEV is forcing us to change our view,
and recognize that AMM swaps really do have a limit price.
