---
title: The gold standard for error handling
date: 2015-04-26 22:04
---

"Good introduction here".

In C#, the standard way to do error handling is with exceptions.
I have written a lot of code in C# that deals with exceptions,
and it is a pain to do it correctly.
Apart from null, it might very well be the biggest design mistake of the language.
(Something about recoverable vs unrecoverable?)
When I first learned about monadic error handling (the `Either` type in Haskell),
I was delighted.
Obviously this was the right approach to error handling, elegant and effective.

Then Rust came along, and I immediately fell in love with it.
It appeared to do everything right that C# did wrong.
Not only did it not have null (an entire class of problems … gone),
it also had unrecoverable failure (now called panic),
and monadic error handling for the cases where errors are not exceptional.

Fast forward a few months.
I am working on a [decoder for the FLAC codec][claxon] in Rust,
and a [library that handles WAV files][hound] to verify it.
Things can fail at several levels:
apart from IO errors, there is the issue of ill-formed data and program errors.
(Program errors? API abuse/misuse? Runtime errors? What is a good word for that?)
Aside from a few toy programs in Scala and Haskell,
it is the first serious project where I get to use monadic error handling.
Mostly it is a breeze,
but there are downsides too.

[claxon]: https://github.com/ruud-v-a/claxon
[hound]:  https://github.com/ruud-v-a/claxon

For error handling/of error handling?
Few options:

- Error codes (C, Go?)
- Exceptions
- Sum types
- Crash + supervisor (Erlang, Akka)

Error codes
-----------
Pros:

- Little runtime cost

Cons:

- Can mix good and bad in one type (e.g. int but < 0 is fail)
  But not in Go, I think
- Easy to forget (though compiler lints)
- Nasty control flow


Exceptions
----------
Pros:

- Control flow focuses on the “happy path” (à la Meijer)
- Separation between good type and failure type
- Good support for debugging, can provide stack trace information

Cons:

- May incur runtime cost
  Especially bad when used for control flow where exceptions are not exceptional,
  e.g. a bad network connection
- Easy to forget catch
- Too eager “catch all”
  Underlying reason is no distinction between recoverable and unrecoverable failures
  (Rumour about new MS language with unrecoverable failures?)
  Other reason is that exception types are implicit (not in Java?),
  how do you know what to catch?
- Boss says app should not crash to user -> catch all -> inconsistent state -> tears everywhere
- Dishonest type system
  Java checked exceptions seem good but do not work in practice?
  Relates to easy forget to catch

Personally dislike exceptions because they are a hole in the type systems. This
is not a problem inherent to exceptions, but it is to every implementation that
I am aware of. The second reason is that (in C#) there is no distinction
between recoverable and unrecoverable exceptions. Possible to catch
`OutOfMemoryException`. On the other hand, situations that are not exceptional
(a dropped network connection, for example) may throw and incur the performance
penalty.

Sum types
---------
Pros:

- Separation between good type and failure type
- Impossible to assume happy path only, must consider failure
- Can be as efficient as error codes

Cons:

- Naively leads to nasty control flow
  But monads and `try!` allow best of both worlds
- Can miss context (e.g. stack trace), harder to pinpoint origin
  Especially if one error type is used in multiple places, when its origin is
  not unique, can be hard to debug
  Although in Rust, could use file and line number macro to add the context
  info, and expose only in debug build, for example.
  In practice, this is a real disadvantage.
  I suffer from this when building Claxon, for example.
  Another problem, is when using `unwrap`, it is not clear _which_ unwrap
  panicked, but when using `.ok().expect()` you lose the error message. How to
  resolve? If let err?
  That is -- which unwrap in the method, if there are multiple.

  This is also biting me with Hound now. Errors propagate upwards perfectly,
  but this also means they only surface at the last moment. There is no
  opportunity to break at the root of the problem,
  which is possible with exceptions. On the other hand, this is partially a
  tooling problem. Maybe it should be possible to break when a method returns
  an `Err` somewhere.

Crash + supervisor
------------------
- Not too familiar with it
- Seems nice for some things, but how do you convey the actual error, reason for failure?

Implementation
--------------
- Ramble a bit about Haskell? And Rust?
- Rust for an other time?
- Rust `Result` with `try!` is awesome
- Mention that `Option` incurs no runtime overhead
- `Result` for recoverable failurs, panic for unrecoverable (though thread watch)
- With `From<E>` different types work like a breeze
- Cannot catch too eager or forget anything, all types explicit
- But still focus on the happy path control flow
