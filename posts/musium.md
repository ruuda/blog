---
title: The story of Musium
date: 2025-09-09
lang: en-US
minutes: ?
synopsis: TODO
run-in: Musium is the music player
---

Musium is the music player that I built for myself.
It runs on a Raspberri Pi,
and I can control it from my local network using a webinterface.
I’ve been using it on a daily basis for years,
but it’s far from finished.
It’s polished in many areas,
but implementing pause and skip is something I haven’t gotten to yet.

<p style="text-align: center">
<img
  src="/images/musium.png"
  alt="The Musium music player."
  style="width: 80%; margin-top: 1em; margin-bottom: 0.5em"
  />
</p>


[Musium][musium] is my ultimate yak shave.
It’s NIH’d across the stack.
It uses [my flac decoder][claxon],
and I implemented the [loudness analysis][bs1770],
normalization,
and high pass filter.
The application is built around a custom in-memory index.
It persists data to SQLite,
for which I wrote [a code generator][squiller]
that generates Rust bindings for SQL queries,
and the frontend is written in PureScript
using a homegrown DOM builder library.
I like polishing it:
the seek bar is not just a line, it renders a waveform,
and the UI is animated throughout.
Musium tracks fairly elaborate statistics about playcounts,
so it can surface interesting music at the right time,
and I developed [a new shuffling algorithm][shuffle] for it.
Musium does _exactly_ what I want it to do,
and that’s very satisfying.
This is its story.

[carnival]: https://lobste.rs/s/0nstyk/join_lobsters_blog_carnival
[claxon]:   https://github.com/ruuda/claxon
[bs1770]:   https://github.com/ruuda/bs1770
[shuffle]:  /2023/an-algorithm-for-shuffling-playlists
[musium]:   https://github.com/ruuda/musium
[squiller]: https://github.com/ruuda/squiller

## The flac decoder

Back in 2014,
Rust started to regularly show up in my news feed.
It got many things right that frustrated me in other languages at the time,
and I was eager to learn.
I started by [porting a path tracer][path-tracer],
but what was a good next project?
What’s a good fit for a low-level memory-safe language?
Codecs.
ClusterFuzz did not exist,
and I regularly had media players segfaulting on corrupted files.
I figured that a video codec would be too ambitious,
but audio should be feasible.

I already had a collection of flac files,
so I wrote [Claxon][claxon],
a decoder for the codec.
In a world where content can disappear from streaming services at any time,
I find it reassuring to have files on a disk that I control,
and to maintain my own software that can decode them.
Nobody can take that away.

In order to test Claxon and play back the decoded samples,
I also had to write [Hound][hound],
a library to read and write wav files.
<!--
(It’s called ‘hound’ because ‘waf’ is the sound a dog makes in Dutch,
but ‘dog’ is not a cool name,
so I called it ‘hound’.)
-->
This was the early days of the Rust ecosystem,
and no library for that existed at the time!
It was pre-1.0,
and just before Rust did
an invasive refactor of IO in the standard library
— a move that Zig would go on to popularize 10 years later.

So I had my flac decoder,
but aside from the fun of writing it,
I didn’t have a direct use case for it.
At that time, I wasn’t planning to write a music player yet.

[claxon]:      https://github.com/ruuda/claxon
[hound]:       https://github.com/ruuda/hound
[path-tracer]: /2014/08/10/writing-a-path-tracer-in-rust-part-1/
[zero]:        /2016/11/30/zero-cost-abstractions/

## Chromecast and the metadata index

A few years later,
I temporarily rented a furnished apartment,
and it came with a multi-room Sonos system.
Until then,
at home I mostly listened to music from my PC.
Having music _in every room_ was an amazing new experience,
and I _needed_ to have this at home when I moved back.
Sonos was expensive though,
and the app was unreliable.

Then there was Chromecast Audio.
In theory, it was perfect.
It played flac,
supported multi-room audio,
and I could control it from my phone.
I could even get an employer discount on it,
so I bought three of them.
But Chromecast was not a full solution.
It streams media over http,
but something external needs to trigger playback;
it doesn’t come with a library browser itself.

So that’s what I set out to build:
an http server that could serve my flac files,
with a library browser that would enqueue tracks on the Chromecast.
I had Claxon that could parse tags from flac files,
and I built an application on top of it that would
traverse my flac files at startup,
read their metadata,
and build an in-memory index that it would serve as json.
I intended to run this on a Raspberry Pi,
so I wanted my code to be efficient
— before generation 3 these things were _slow_,
and memory was counted in megabytes.
All of this was of course a premature optimization,
but it was a lot of fun to build!

At the time <abbr>SSD</abbr>s were still quite expensive,
so I kept my collection on a spinning disk.
Optimizing disk access patterns was a fun journey.
By bumping `/queue/nr_requests` in `/sys/block`,
and reading with hundreds of threads,
the IO scheduler can do a _much_ better job,
and I could index 16k files in 70 seconds with a cold page cache,
down from 130s before optimization.

Now I had a server that could serve my library,
and I could start tracks with [rust-cast],
but that’s still not a full music player.

[rust-cast]: https://github.com/azasypkin/rust-cast

## Adding a webinterface

<!-- Context for this part:
-->

I wanted a library browser that I could use from my phone,
as well as desktop.
How hard could it be in 2018?
I considered various ways to build an Android app,
as well as a web app.
None of them were great.
A native Android app would not run easily on my desktop.
Flutter’s opinionated tooling was incompatible with Nix,
and looked like I’d spend more time
fixing my development environment every few months,
than writing code.
JavaScript is unsuitable for anything over a few hundred lines of code,
and TypeScript had a dependency on the NPM and nodejs ecosystem
for which I have a zero-tolerance policy in my personal projects.
(I’m excited for [typescript-go], but it did not exist at the time.)
I started out with Elm,
but I found it too constraining in how it interacts with JavaScript,
which I needed to use Cast.
So in 2019, I switched to PureScript,
and that one stuck.

I did really like Elm’s approach to defining DOM nodes,
similar to blaze-html in Haskell.
The PureScript counterpart of that was [Halogen][halogen],
so that’s what I started out with.
In July 2019,
for the first time I was able to cast a track from my app,
using the Cast API that Chromium exposes.

## An imperative frontend framework

I quickly found myself constrained by Halogen.
It’s model where an application is a pure fuction `State -> Html` works well
when all changes are instant,
but I found it unsuitable for what I wanted to do.
In the browser,
DOM nodes have state like selections and CSS animations.
It’s not enough to give a declarative specification of the desired DOM tree,
and let a library apply the diff between the current and new tree.
I need control over the nodes.
Maybe I was holding Halogen wrong,
but I wrote my own DOM manipulation library instead,
and I’ve been quite pleased with it it ever since.
I later used it in [my plant watering tracker][sempervivum] as well.
Here’s the function that renders the volume slider:

```purescript
type Slider =
  { bar :: Element
  , label :: Element
  , buttonDec :: Element
  , buttonInc :: Element
  }

addSlider :: String -> String -> Html Slider
addSlider textDec textInc = Html.div $ do
  Html.addClass "volume-control"
  elements <- Html.div $ do
    Html.addClass "indicator"
    Html.div $ do
      label <- Html.div $ do
        Html.addClass "volume-label"
        ask
      bar <- ask
      pure $ { bar, label }

  buttonDec <- Html.button $ do
    Html.addClass "volume-down"
    Html.text textDec
    ask

  buttonInc <- Html.button $ do
    Html.addClass "volume-up"
    Html.text textInc
    ask

  pure { bar: elements.bar, label: elements.label, buttonDec, buttonInc }
```

It looks almost declarative,
and it preserves this workflow where rendering
is a pure function from state to DOM nodes.
If you look closely though, it’s imperative.
`Html` is a reader monad that exposes the surrounding node.
Functions like `div` and `li` construct a new node,
run the body with that node as context,
and finally call `appendChild`
to add the new node to its parent.
We _can_ use this approach to rebuild the entire tree,
but with `ask` we can also store the node,
and later make more targeted mutations inside it:

```purescript
updateSlider :: Slider -> Number -> String -> Effect Unit
updateSlider slider percentage label = do
  Dom.setWidth (show percentage <> "%") slider.bar
  Html.withElement slider.label $ do
    Html.clear
    Html.text label
```

This way of building the UI is now six years old,
and every time I need to edit the UI,
I’m surprised by how easy it is to change.

[typescript-go]: https://github.com/microsoft/typescript-go
[halogen]:       https://github.com/purescript-halogen/purescript-halogen
[sempervivum]:   https://github.com/ruuda/sempervivum

<!--
PureScript:        https://github.com/ruuda/musium/commit/df557220d32ffd2bd3cca44676e7a51760188f55
First cast:        https://github.com/ruuda/musium/commit/ccc52128a7db721c1dec53f25b73c40211fcf324
Halogen to custom: https://github.com/ruuda/musium/commit/bfefa8008e5e1d2ee20c2cb5cf737cea2f452159
-->

## Chromecast is the most unreliable software ever

Cast from inside app or server-side.
Aside from the UI,
it needed to support the Cast protocol.


