---
title: Model facts, not your problem domain
date: 2020-06-07
minutes: 1
lang: en-GB
synopsis: When requirements change, an append-only data model of immutable facts is more useful than a mutable data model that models the problem domain.
run-in: When requirements change
---

When requirements change,
an append-only data model of immutable facts is more useful
than a mutable data model that models the problem domain.

When I started building [a plant watering tracker](https://github.com/ruuda/sempervivum)
about six weeks ago,
I almost made the mistake of giving its database a mutable ‘last watered’ column.
Fortunately I realised in time to use an append-only table with watering events.
At the time I had no use for historical data,
the only things required for a watering reminder
are the watering interval and last watered date.
When mutability is the default,
storing all watering events seems ridiculous.
But when immutable is your default,
keeping historical data is the natural thing to do:
mutating rows in place is a storage space optimisation,
and in my case it would be entirely premature.

Now, a few weeks later, I want to make the watering schedule adaptive.
If the watering reminder is consistently two days early,
probably the default interval is too short for that plant.
For this feature I do need historical data.
Fortunately, I already have that data,
so the new feature is useful right away.
But more importantly,
no schema changes are required to support the new feature.

My point is not “collect all the things, it might be useful some day.”
My point is that a database that stores facts (watering events)
is in a much better position to deal with changing requirements,
than a database that was modelled after the initial requirements
(producing a reminder a fixed number of days after watering).
When requirements change,
the facts of the situation do not,
so a database of facts remains useful.

Further reading
---------------

My preference for immutable data is heavily informed by the work of Rich Hickey,
who explains this much better than I ever could in a short blog post.
The following talks are worth watching:

 * [The Value of Values](https://www.infoq.com/presentations/Value-Values/)
 * [Simple Made Easy](https://www.infoq.com/presentations/Simple-Made-Easy/)
 * [Deconstructing the Database](https://www.infoq.com/presentations/Deconstructing-Database/)
