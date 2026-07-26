---
title: Hosting without big tech
date: 2026-06-25
lang: en-US
synopsis: TODO
minutes: ??
run-in: I bought my first domain in 2006
---

I bought my first domain in 2006,
with a shared hosting package from a local Dutch hosting company called _Hosting Discounter_.
My dad helped me buy it.
He had found the company
through an ad in the computer magazine he subscribed to.
I put a website on it
about the games I was building with a program called _Game Maker_,
the gateway drug that got me into programming.
Little did I know about how the web worked under the hood,
but I could write a website in HTML and CSS,
and copy the files to the server over FTP.
A year later I rewrote the website in PHP.
What can I say, we were all young once.

Fast-forward to 2011.
I was studying at university
and working a side job as a C# programmer.
<!-- TODO
for a company that built software that ran on-prem.
-->
/r/programming was blooming.
It was full of interesting blogs,
and I wanted one too.
So in 2012 I bought ruudvanasseldonk.com,
and I put a Jekyll blog on it.
By now I was comfortable with Linux and Apache,
but shared hosting still worked fine for my static site.

[Octopress]: https://octopress.org/

## Everything moves to the cloud

Fast-forward to 2015.
After an internship at Google,
I returned to The Netherlands to do a master’s degree.
As a side job I joined a local SaaS startup.
We were hosting our software on virtual machines
with a Dutch hosting company,
but they were unreliable.
At the same time,
Google offered startups free credits on Google Cloud.
[So we moved everything][channable-cloud].
It worked well.
The virtual machines were stable,
we were pleased.

[channable-cloud]: https://www.channable.com/tech/how-we-moved-to-google-cloud-using-consul-and-zero-tier

At Google I worked on Chromium,
and I learned a lot about improving page load times.
I wanted to optimize my blog,
but for that I needed to be able to configure my webserver myself.
I set one up with the tools I was familiar with:
Ubuntu, Nginx, and Google Cloud.
They offered this `f1-micro` instance for free in the their US regions,
and the majority of my visitors were from the United States,
so hosting there optimized for the common case.
The `f1-micro` served me well
even as some of my posts made it to the Hacker News frontpage.
You can serve a _lot_ of static pages with half a CPU core.

I considered adding Cloudflare,
but then I would again not be in control of my HTTP server.
They would also be able to MITM my readers,
and I estimated that for my regular traffic volume,
cache misses would be common,
so I might as well serve my site directly.
I did adopt Cloudflare as my DNS server.
It felt more professional
than these Dutch hosting companies with their 2007-era control panels.

It was not just me,
or the startup I worked for.
_Everyone_ around me was already in, or moving to the cloud.
People I spoke with at meetups,
blog posts,
talks at conferences …
cloud was the way.
If you were still on-prem or colocating, you were a dinosaur.

## Everything else disappeared

By 2024,
something funny had happened.
Nobody knew how the lower levels of the stack worked any more.
Clouds had expanded beyond simple virtual machines to container managers,
and then serverless.
Everything became managed.
At that time I was working for a company
that was doing platforms engineering work,
primarily on bare metal,
and we found it difficult to hire good engineers.
<!-- -->

## Friendship ended with United States

On February 6, 2025,
Trump ordered [sanctions] against Karim Khan,
chief prosecutor at the International Criminal Court in The Hague.
The court was a customer of Microsoft.
Microsoft complied with the order by blocking the prosecutor’s account.
Khan and others lost access to their email,
which [disrupted][icc-disrupt] the court’s operations for months.

This was a wake-up call.
By January 2026,
nu.nl’s editorial board [wrote][nu-board]
that journalism needs to start taking the American threat seriously:
three weeks into the new year,
Trump had already abducted a foreign head of state
and threatened military action against a NATO ally.
Two days after the article, he started another war.
Government and the public sector are not the only ones feeling the heat:
business are dealing with unpredictable tariffs,
and lately Trump has taken up the liberty
to decide who gets access to Anthropic’s latest models.
American corporations are simply no longer a reliable business partner.

This new distrust for the United States is a big challenge for Europe.
We’re finding our entire society completely dependent on American big tech,
scrambling to break free
before Trump starts using that dependency as political leverage.
At the same time,
this is a great opportunity for European tech companies
— an opportunity that I’m going to jump on.
Before I can share more about that though,
there is something I need to fix:
it would be hypocritical
to share such an announcement
on a blog that is hosted in the United States,
with an American hyperscaler.

[sanctions]:   https://www.whitehouse.gov/presidential-actions/2025/02/imposing-sanctions-on-the-international-criminal-court/
[icc-disrupt]: https://apnews.com/article/icc-trump-sanctions-karim-khan-court-a4b4c02751ab84c09718b1b95cbd5db3
[berthub]:     https://berthub.eu/articles/
[nu-board]:    https://www.nu.nl/blog/6382843/journalistiek-moet-amerikaanse-dreiging-serieus-nemen.html

## Hosting on European infrastructure

The webserver itself was easy enough to move.
You can get virtual machines anywhere,
and it has no special requirements.
I ended up running the webserver on [Exoscale],
but I also use <abbr>VM</abbr>s from [Upcloud] and [Cherry Servers].
They are for sure not as mature as GCP and AWS:
none of them even support 2FA using a Yubikey.
For <abbr>VM</abbr>s they work fine though,
and in the four months so far
I haven’t had any reliability issues.
As a bonus,
I now also support IPv6,
something that was not available on Google Cloud when I first set things up there.

For DNS I was still dependent on Cloudflare.
By now I had acquired enough domains
that I didn’t want to manage DNS
in the various webinterfaces of the different registrars I used.
<!-- I could have gone with [Bunny](https://bunny.net/dns/), -->
Fortunately by now I was now confident enough
in platforms engineering to run my own DNS servers,
so I set up two [NSD] instances.
I also built a [new deployment tool](/2026/deptool/)
to manage this tiny cluster of virtual machines,
because there is no way I’m using Ansible when nobody is paying me for it.

When you host a nameserver for your own domain,
there is a circular dependency:
if the nameserver for `ruuda.nl` is `ns1.ruuda.nl`,
then who do you ask for the address of `ns1.ruuda.nl`?
The answer to this is: the nameserver for `.nl`.
It serves not just `NS` records for you,
but also `A` and `AAAA` records.
These are called _glue records_,
and the boutique registrars do not have a self-serve way to configure them.
I ended up migrating ruuda.nl
to exactly the hosting company we abandoned for Google Cloud 10 years ago,
and I have to admit that their DNS control panel is pretty good.
It automatically detects when you need glue records,
and allows you to set them.

My blog has one remaining dependency on an American service: Let’s Encrypt.
That’s a concerning one,
because as they clarified in the revised subscriber agreement from June 2026,
they have to comply with US sanctions,
and a certificate authority would be an effective way to target an organization,
or even an entire country.
For my personal blog I’m okay with this for now,
but …
No actually, maybe I can just fix this right now?

[Exoscale]:       https://www.exoscale.com/
[Upcloud]:        https://upcloud.com/
[Cherry Servers]: https://www.cherryservers.com/
[NSD]:            https://nlnetlabs.nl/projects/nsd/about/
