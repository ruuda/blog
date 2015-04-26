---
title: The gold standard for error handling
date: 2015-04-26 22:04
---
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

Sum types
---------
Pros:

- Separation between good type and failure type
- Impossible to assume happy path only, must consider failure
- Can be as efficient as error codes

Cons:

- Naively leads to nasty control flow
  But monads and `try!` allow best of both worlds

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
