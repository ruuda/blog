---
title: Hosting without hyperscalers
date: 2026-10-06
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
Little did I understand about how the web worked under the hood,
but I could write a website in HTML and CSS,
and use FTP to copy the files to the server.
Later I added some PHP.
That’s how you built a website back then.

Today,
20 years later,
the web no longer works likes this.
Shared hosting still exists,
but like PHP,
it feels like a relic of a different time.
Instead of local companies that advertise in paper magazines,
we ended up with a small number of giants
that almost everybody depends on:
the _hyperscalers_.
And somehow, they are all based in the United States.
As our society grew dependent on the web and the internet,
this hosting monoculture has created a precarious situation.
So after 10 years of hosting my blog on Google Cloud,
I’m now back hosting on European infrastructure.
Let’s dive in.

## Starting a blog

Five years after I bought that first domain,
I was studying at university
and working a side job as a <abbr>.NET</abbr> programmer.
The software we built ran on-prem,
on our customers’ servers and <abbr>PCs</abbr>.
Reddit was browsable without an account,
and /r/programming was booming.
It was full of interesting blogs,
and I wanted one too.
So a year later I bought ruudvanasseldonk.com
and put a Jekyll blog on it.
<!-- with a theme inspired by the lovely [Octopress]. -->
By now I was comfortable with Linux and Apache,
but shared hosting still worked fine for my static site.
In 2015 I [added][https] a TLS certificate.
I paid for it, because Let’s Encrypt did not exist yet.
But even TLS worked fine on shared hosting.

[Octopress]: https://octopress.org/
[https]: /2015/encryption-by-default

## Everything moves to the cloud

After my graduation
I did an internship at the London office of a large tech company,
and then I returned to The Netherlands for a master’s degree.
As a side job I joined a local SaaS startup.
We were hosting our software on virtual machines
with a Dutch hosting company,
but their <abbr>VMs</abbr> were unreliable.
Meanwhile,
Google Cloud offered startups free credits.
[So we moved everything][channable-cloud].
It worked well:
the virtual machines were stable
and we were pleased.

[channable-cloud]: https://www.channable.com/tech/how-we-moved-to-google-cloud-using-consul-and-zero-tier

During my internship earlier I worked on a web browser,
and through that I learned a lot about page load times.
I wanted to optimize my blog,
but for that I needed more control over my webserver.
So I set one up with the tools I was now familiar with:
Ubuntu, Nginx, and Google Cloud.
Google offered one `f1-micro` instance for free in their US regions.
The majority of my visitors were from the United States,
so hosting there optimized for the common case.
Sometimes a post would make it to the Hacker News frontpage,
but the `f1-micro` never broke a sweat.
You can serve a _lot_ of simple http requests on a fifth of a CPU core!

<!--
I considered adding Cloudflare,
but then I would again not be in control of my http server.
They would also be able to MITM my readers,
and I estimated that for my regular traffic volume,
cache misses would be common,
so I might as well serve my site directly.
I did adopt Cloudflare as my DNS server.
It felt more professional
than the local registrars with their 2007-era control panels.
-->

It was not just me,
or the startup I worked for.
_Everyone_ around me was already in, or moving to the cloud.
People I spoke with at meetups,
blog posts,
talks at conferences …
cloud was the future.
If you were still on-prem or colocating, you were a dinosaur.

## Everything else disappeared

By 2024,
something funny had happened.
It seems like people had forgotten how the lower levels of the stack worked.
Clouds dominated.
They had moved well beyond virtual machines
to container managers and serverless.
Everything became managed and metered.
I worked for a company
that was doing platforms engineering work primarily on bare metal,
which felt rare.
We found it difficult
to find people who still understood
how to make things run without managed services.
Everybody took the existence of the hyperscaler cloud for granted.

## Friendship ended with United States

On February 6, 2025,
Trump ordered [sanctions] against Karim Khan,
chief prosecutor at the International Criminal Court in The Hague.
The court was a customer of Microsoft.
Microsoft complied with the order by blocking the prosecutor’s account.
The ban [disrupted][icc-disrupt] the court’s operations for months.

This was a wake-up call.
By January 2026,
the editorial board of a major Dutch news site [wrote][nu-board]
that journalism needs to start taking the American threat seriously:
that country’s rapid devolution into an autocratic regime
is not going to just blow over.
Three weeks into the new year,
Trump had already threatened military action against a NATO ally
and abducted a foreign head of state,
violating international law in the process.
European governments have become nervous
about their dependence on US-based big tech.
<!--The Dutch government now has a state secretary for digital sovereignty.-->
Businesses are feeling the effects too.
They’ve been dealing with unpredictable tariffs,
and lately Trump has taken up the liberty
to decide who gets access to Anthropic’s latest models.
American corporations are simply no longer reliable business partners.

This newfound distrust for the United States
is an immense challenge for Europe.
We’re finding our entire society _completely_ dependent on American big tech,
scrambling to break free
before Trump starts pushing harder.
At the same time,
this presents a great opportunity for European tech companies.
An opportunity that I’m going to jump on.
Before I share more about that though,
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

The webserver itself was easy to move.
You can get virtual machines anywhere,
and Nginx serving a small site consumes virtually zero resources.
I ended up running the webserver on [Exoscale],
but I also use <abbr>VM</abbr>s from [Upcloud] and [Cherry Servers].
Their platforms are not as advanced as GCP and AWS
(none of them support hardware security keys for 2FA for example),
but the <abbr>VM</abbr>s work fine,
and in the seven months so far
I haven’t had any reliability issues.
As a bonus,
my blog now also supports IPv6,
something that was not available on Google Cloud when I first set things up there.

For DNS I used to use Cloudflare.
Throughout the years I had acquired enough domains
that I didn’t want to manage DNS
in the various webinterfaces of the different registrars I used,
and Cloudflare was convenient.
But they fall under US jurisdiction, so they had to go.
Fortunately by now I was now confident enough
in platforms engineering to run my own DNS servers,
so I set up two [NSD] instances.
(And I [built a new deployment tool](/2026/deptool/) to manage them,
because there is no way I’m using Ansible when nobody is paying me for it.)
After some initial hurdles around getting my registrar to add glue records,
running my own nameservers has been completely uneventful.
While at it,
I also changed the canonical domain of my blog from a .com domain to ruuda.nl.
Now I was almost free of American services.
_Almost._

[Exoscale]:       https://www.exoscale.com/
[Upcloud]:        https://upcloud.com/
[Cherry Servers]: https://www.cherryservers.com/
[NSD]:            https://nlnetlabs.nl/projects/nsd/about/

<!--
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
-->

## Let’s Encrypt

My blog still depended on one American service: Let’s Encrypt.
To be clear,
I have nothing but respect for Let’s Encrypt.
It would be an overstatement to say that they singlehandedly
made encryption on the web commonplace,
but not _much_ of an overstatement.
But alas,
the organization behind Let’s Encrypt
is registered in the United States,
and as they clarified in the June 2026 subscriber agreement update,
that means they have to comply with US-imposed sanctions.
A certificate authority is an effective way to target an organization,
or even an entire country,
especially now that so much software
takes the availability of Let’s Encrypt for granted.
So even though I deeply respect Let’s Encrypt,
I needed a plan&nbsp;B.

There are other certificate authorities that offer ACME,
but I could find only one that is unambiguously outside of US control:
[Actalis][actalis], based in Italy.
I have very mixed feelings about Actalis.
Their website fails Firefox’ TLS handshake half of the time,
<!-- with a `PR_END_OF_FILE_ERROR`. -->
<!-- they don’t support altnames, -->
they don’t document the correct value for your [CAA records][caa] anywhere
(turns out the identifying domain is actalis.it, not .com),
they send me notifications in Italian,
and they encourage you to paste your private key into
[a form protected by a captcha][keymatch].
(Which, after diving into it,
turns out to handle the cryptography locally in the browser,
but _also_ makes a request to Google every time you submit!?)
I’m not impressed, but it does work.
I now use Actalis for some of my domains,
and I can easily change my config to switch the other domains should I ever need to.
That’s sufficient:
digital autonomy does not mean abandoning US-based services entirely.
I just need to make sure that I can quickly switch
if Let’s Encrypt ever becomes unavailable to me.

[actalis]:  https://www.actalis.com/
[keymatch]: https://extwebra.actalis.it/portal/uapub/tools/keymatch
[caa]:      https://letsencrypt.org/docs/caa/

## Towards digital autonomy

It’s been 20 years since I built my first website,
and many things have changed in those 20 years.
For the first 10 years of my online presence,
I hosted my websites on shared hosting with a local provider.
Then cloud took over,
and for the next 10 years
I hosted in the cloud,
with an American hyperscaler.
In Europe,
that age is now coming to an end too.
As Trump is forcing us to face
the risks of a monoculture of dependencies on foreign big tech,
we are entering the age of digital sovereignty and autonomy.

A simple blog is easy enough to move.
More complex digital infrastructure will be more challenging,
but not impossible.
There is no shortage of raw compute in Europe,
but the providers haven’t quite reached _hyper_ scale,
and they don’t offer such a vast array of vertically integrated services
as the hyperscalers do.
Secretly, I hope things can mostly stay that way.
Centralization has advantages,
but it also creates fragility.
Rather, I want to see a future where many hardware providers can thrive,
with the services managed by open source software on top.
That way,
we stay in control of the digital infrastructure we depend on.
[That’s what I am building towards.][Soverity]

[Soverity]: https://soverity.eu
