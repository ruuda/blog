---
title: An API for my Christmas tree
break: my Christmas
date: 2017-11-12
synopsis: ???.
run-in: It is almost
---

It is almost that time of the year again.
Time to bring out the Christmas tree,
the Arduino, and the RGB LED strand.
Let’s have a bit of fun by connecting my tree to the internet.

Hardware
--------

The [<abbr>LED</abbr>s I had lying around][leds]
are the perfect lights for a Christmas tree
-- the brightly coloured wires just give it that nice touch of do-it-yourself electronics.
The <abbr>LED</abbr>s can be addressed individually using an Arduino Uno.
Adafruit provides an [Arduino program][adalight]
that accepts colour data over a serial connection,
and drives the LED strand.
The only thing left is to feed it interesting colours.

[leds]:     https://www.adafruit.com/product/322
[adalight]: https://github.com/adafruit/Adalight

Software
--------

I wrote a small program that computes the colour of every LED
as a function of LED number and time.
Different functions can produce effects such as blinking,
or an animated rainbow.
The program runs on a local machine,
to which the Arduino is connected via USB.
I did not want to run an internet-exposed server on this machine,
so the program acts as a client,
and receives updates from a server about the colour function to use.

The server program exposes a simple REST API
with endpoints to change the color function,
or to temporarily blink in a given color.
On a different port the server accepts TCP connections from the client progam.
Over this connection, the server broadcasts changes to the color function.
The server supports basic access control:
the API is protected by a password,
and only available over https.
The connection between the client program and server is unencrypted though,
and there is no authentication there.
Beware of men in the middle messing with your Christmas lights.

The client and server program are free software,
[available on GitHub][ct-gh].
Both are written in Haskell,
so a simple `stack build` will produce two binaries
with no runtime dependencies apart from a few system libraries.
I copied over the server binary to a cheap cloud instance,
set up a Letsencrypt certificate,
and I was good to go.
An example systemd unit is included in the repository.

[ct-gh]: https://github.com/ruuda/christmas-tree

Deployment
----------

At this point I could send an API call to the server,
and the pattern in the tree would change.
That was pretty cool --
somehow making lights blink always feels magical.
But what do you do with an internet-controlled Christmas tree?
You hook it up to the build system, of course!

I deployed the lights at work and put this in our `.travis.yml`:

```yml
after_success:
  # Make the Christmas tree blink green.
  - curl -X POST 'https://chainsaw:pass@tree.example.com/blink?color=00ff00&seconds=10'

after_failure:
  # Make the Christmas tree blink red.
  - curl -X POST 'https://chainsaw:pass@tree.example.com/blink?color=ff0000&seconds=10'
```

This even turned out to be semi-useful for a short while
-- until my colleagues found out
that they could also make the tree blink red
from their local workstations ...
