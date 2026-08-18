---
title: Hosting without hyperscalers
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
Little did I understand about how the web worked under the hood,
but I could write a website in HTML and CSS,
and copy the files to the server over FTP.
Later I added some PHP.
That’s just how you built a website back then.

Today,
20 years later,
the web no longer works likes this.
Shared hosting still exists
— my original website is still online! —
but it feels like a relic of a different time.
Instead of local companies that advertise in paper magazines,
we have a few giants that almost everybody depends on.
The _hyperscalers_.

<!-- Meh where is this post going? -->

Fast-forward to 2011.
I was studying at university
and working a side job as a C# programmer.
The software we built ran on-prem, on our customer’s servers and <abbr>PCs</abbr>.
/r/programming was blooming.
It was full of interesting blogs,
and I wanted one too.
So in 2012 I bought ruudvanasseldonk.com,
and I put a Jekyll blog on it.
By now I was comfortable with Linux and Apache,
but shared hosting still worked fine for my static site.

[Octopress]: https://octopress.org/

## Everything moves to the cloud

After I graduated I did an internship at a large tech company,
and then I returned to The Netherlands for a master’s degree.
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

During my internship I worked on a web browser,
and I learned a lot about improving page load times.
I wanted to optimize my blog,
but for that I needed to be able to configure my webserver myself.
I set one up with the tools I was familiar with:
Ubuntu, Nginx, and Google Cloud.
They offered this `f1-micro` instance for free in the their US regions.
The majority of my visitors were from the United States,
so hosting there optimized for the common case.
Occasionally a post would make it to the Hacker News frontpage,
but the `f1-micro` didn’t break a sweat as the crowd tried to hug it to death.
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
Everybody had forgotten how the lower levels of the stack worked.
Clouds had expanded beyond simple virtual machines to container managers,
and then serverless.
Everything became managed.
At that time I was working for a company
that was doing platforms engineering work,
primarily on bare metal,
and we found it difficult
to hire engineers who understood the lower levels of the stack.
Platforms engineering had become synonymous with writing yaml.

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
the editorial board of a popular Dutch news site [wrote][nu-board]
that journalism needs to start taking the American threat seriously:
three weeks into the new year,
Trump had already abducted a foreign head of state
and threatened military action against a NATO ally.
Two days after the article, he started another war.
Goverment is not the only sector under pressure:
business are dealing with unpredictable tariffs,
and lately Trump has taken up the liberty
to decide who gets access to Anthropic’s latest models.
American corporations are simply no longer a reliable business partner.

This new distrust for the United States is a big challenge for Europe.
We’re finding our entire society completely dependent on American big tech,
scrambling to break free
before Trump starts exploiting that dependency for political leverage.
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
They are not as mature as GCP and AWS,
but for <abbr>VM</abbr>s they work fine,
and in the five months so far
I haven’t had any reliability issues.
As a bonus,
my blog now also supports IPv6,
something that was not available on Google Cloud when I first set things up there.

For DNS I was still dependent on Cloudflare.
Throughout the years I had acquired enough domains
that I didn’t want to manage DNS
in the various webinterfaces of the different registrars I used.
<!-- I could have gone with [Bunny](https://bunny.net/dns/), -->
Fortunately by now I was now confident enough
in platforms engineering to run my own DNS servers,
so I set up two [NSD] instances.
I also built a [new deployment tool](/2026/deptool/)
to manage this tiny cluster of virtual machines,
because there is no way I’m using Ansible when nobody is paying me for it.
After some initial hurdles around getting my registrar to add glue records,
running my own nameservers has been pretty uneventful.

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

My blog had one remaining dependency on an American service: Let’s Encrypt.
To be clear,
I have nothing but respect for Let’s Encrypt.
It would be an overstatement to say that they singlehandedly
made encryption on the web commonplace,
but not _much_ of an overstatement.
Unfortunately,
ISRG — the organization behind Let’s Encrypt —
is registered in the United States,
and as they clarified in the June 2026 subscriber agreement update,
that means they have to comply with US sanctions.
A certificate authority would be an effective way to target an organization,
or even an entire country,
especially now that so much software
takes the availability of Let’s Encrypt for granted.
So even though I love Let’s Encrypt,
it’s becoming too risky to not have a plan&nbsp;B.

There are other certificate authorities that offer ACME,
but I could find only one that is unambiguously outside of US jurisdiction:
[Actalis][actalis], based in Italy.
Where Let’s Encrypt radiates competence,
Actalis makes me question theirs.
Let’s Encrypt maintains an interesting engineering blog.
Actalis’ website fails Firefox’ TLS handshake half of the time.
<!-- with a `PR_END_OF_FILE_ERROR`. -->
Let’s Encrypt has [stellar documentation][le-docs].
Actalis fails to mention
that you’re supposed to set your CAA record to actalis.it
despite the ACME API being on their .com domain.
Let’s Encrypt is always pushing to make things more secure.
Actalis encourages you to paste your private key into
[a form protected by a captcha][keymatch].
(It verifies locally in the browser,
but it also makes a request to Google when you submit.)
So I’m not impressed, but in the end I did get it to work.
I now use Actalis for some of my domains,
and I can easily change my config to switch the other domains if I ever need to.
That’s enough:
I don’t need to abandon Let’s Encrypt completely,
I just need to make sure that I can quickly switch
if it ever becomes unavailable to me.

[actalis]:  https://www.actalis.com/
[keymatch]: https://extwebra.actalis.it/portal/uapub/tools/keymatch
[le-docs]:  https://letsencrypt.org/docs/caa/

## Conclusion

Todo, write some conclusion.
