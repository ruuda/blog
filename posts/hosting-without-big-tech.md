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
through an ad in the computer magazine he subscribed to, _Computer Idee_.
I put a website on it
about the games I was building with a program called _Game Maker_,
the gateway drug that got me into programming.
Little did I know about how the web worked under the hood,
but I could write a website in HTML and CSS,
and copy the files to the server over FTP.
A year later I rewrote the website in PHP.
What can I say, we were all young once.

Fast-forward to 2011.
I was studying at university,
and I was working a side job as a C# programmer.
/r/programming was blooming.
It was full of interesting blogs,
and I wanted one too.
So in 2012 I bought ruudvanasseldonk.com,
and I put a Jekyll blog on it.
By now I was comfortable with Linux and Apache,
but shared hosting worked fine for my static site.

[Octopress]: https://octopress.org/

## Everything moves to the cloud

Fast-forward to 2015.
After an internship at Google,
I returned to The Netherlands to do a master’s degree.
As a side job I joined a local startup.
We were hosting our software on virtual machines
rented from a Dutch hosting company,
but they were unreliable.
At the same time,
Google offered startups free credits on Google Cloud.
[So we moved everything][channable-cloud].
It worked well,
the virtual machines were stable,
and we were pleased.

[channable-cloud]: https://www.channable.com/tech/how-we-moved-to-google-cloud-using-consul-and-zero-tier

Around this time,
I wanted to have more control over my webserver.
At Google I had worked on Chromium,
so by now I knew a lot about improving page load times,
but for that I needed to be able to configure my webserver myself.
I set one up with the tools I was familiar with:
Ubuntu, Nginx, and Google Cloud.
Google Cloud offered this `f1-micro` instance for free in the their US regions,
and the majority of my visitors were from the United States anyway.

I considered adding Cloudflare,
but then I would again not control my HTTP server,
it would MTM my readers,
and I estimated that for my traffic volume,
cache misses would be common,
so I might as well serve my site directly.
I did adopt Cloudflare as my DNS server.
It felt more professional
than these Dutch hosting companies with their 2007-era control panels.

