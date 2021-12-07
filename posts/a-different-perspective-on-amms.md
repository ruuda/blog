---
title: A perspective shift on AMMs
date: 2021-12-07
minutes: ?
synopsis: TODO
run-in: It takes time
---

It takes time for new ideas to be properly understood.
Often the way we first formulate an idea,
is in hindsight not the simplest or the most elegant way,
and a change of perspective can lead to new insights.
TODO.
I think we are in this situation with automated market makers right now.

Automated market makers (<abbr>AMM</abbr>s for short) are a relatively recent development,
and although the maths behind swaps is well understood,
I don’t think that this is the full picture.
[Flash liquidity in Uniswap v3][flashlp] is a clear example of this:
it’s a mechanism that admits a profitable stategy which was not intended by the developers,
and this profit comes at the expense of the intended use case (regular liquidity providing).
In a sense, it’s a game-theoretic vulnerability in the protocol.
Sometimes these flaws can be fixed at the protocol level,
but they can also indicate a flaw in our understanding.

[flashlp]: https://twitter.com/revertfinance/status/1409642606082940930

Limit orders, not market orders
-------------------------------

My original view of AMM swaps was that they are like market orders:
you trade asset X for asset Y,
and you pay whatever the pool’s current price is at the time the swap executes.
The price at the time when the swap executes
may be different from the price at the time when you entered the order.
