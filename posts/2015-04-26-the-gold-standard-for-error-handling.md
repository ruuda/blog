---
title: The gold standard for error handling
date: 2015-04-26 22:04
---

"Good introduction here".

In C#, the standard way to do error handling is by using exceptions.
I have written plenty of code in C# that deals with exceptions,
and it is a pain to do it correctly.
Apart from null, it might very well be the biggest design mistake of the language.
(Something about recoverable vs unrecoverable?)
When I first learned about monadic error handling (the `Either` type in Haskell),
I was delighted.
Obviously this was the right approach to error handling, elegant and effective.

Along came Rust, and I immediately fell in love with it.
It appeared to do everything right that C# did wrong.
Not only did it feature the absence of null (an entire class of problems … gone),
it also had unrecoverable failures (now called panics),
and monadic error handling for the cases where errors are not exceptional.

Fast forward a year.
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

Who throws what?
----------------
The biggest problem with exceptions, in my opinion,
is that they are an escape hatch in the type system.
As Erik Meijer likes to call it --- they make a type system [dishonest][meijer2008].
At least, in C# they do.
Some languages in theory implement exceptions in a more honest manner,
but they suffer from the same problem in practice.

[meijer2008]: https://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-Functional-Programming

The consequence, when you want to write robust software in C#,
is that you constantly have to keep an [MSDN][msdn] tab open.
Can this function throw?
Should I catch a `PathTooLongException` here?
And this is only for library functions, which are well-documented!
Imagine dealing with a third-party library,
or function calls a few layers deep in your own application:
(Am I suggesting my own application is not documented very well?)
at a certain point, you have to assume _everything may throw_.

[msdn]: https://msdn.microsoft.com/en-us/library/gg145045.aspx
[getfullpath]: https://msdn.microsoft.com/en-us/library/system.io.path.getfullpath.aspx

Algebraic data types solve this issue in a surprisingly clean way.
I will use Rust as an example here,
but the same machinery is available in many more languages,
such as Scala, Haskell, and maybe even [a future version of C#][csharppattern].
Consider the following method in C#:

```csharp
class NoPredecessorException : Exception { }

static uint Predecessor(uint x)
{
  if (x == 0)
  {
    throw new NoPredecessorException();
  }
  else
  {
    return x - 1;
  }
}
```

The Rust equivalent would be the following:

```rust
struct NoPredecessor;

fn predecessor(x: u32) -> Result<u32, NoPredecessor> {
    if x == 0 {
        Err(NoPredecessor)
    } else {
        Ok(x - 1)
    }
}
```

(Is this a bad example because it has only a single variant?)
Instead of returning `u32`, the function returns a `Result`,
which is either `Ok(x)` where `x` is an `u32`,
or `Err(NoPredecessor)`.

The function may be called in two scenarios:
either we deal with the problem at the call site,
or we escalate the problem. (And here I suddenly switched from ‘I’ to ‘we’.)
The default in C# is to escalate.
If you don’t catch the exception,
it will continue to unwind stack frames until it encounters a handler.
Handling can be done with a simple catch.
For example, we could implement a [Kelvin versioning][kelvinversioning] scheme
like so:

```csharp
string NextVersionString(uint currentVersion)
{
  return Predecessor(currentVersion).ToString();
}

void PrintNextVersion(uint currentVersion)
{
  try
  {
    Console.WriteLine("The next version is {0}.", Predecessor(currentVersion));
  }
  catch (NoPredecessorException)
  {
    Console.WriteLine("It is impossible to release a new version.");
  }
}
```

One of the problems with escalating by default,
is that it is easy to ignore failure.
Blah rant about easy to forget.
In contrast, the Rust compiler refuses to compile the following function:

```rust
fn next_version_string(current_version: u32) -> String {
    use std::string::ToString;
    u32::to_string(&predecessor(current_version))
}
```

“Error: mismatched types,” it says.
“Expected `&u32`, found `&Result<u32, NoPredecessor>`.”

Blah the match, show `try!` etc.
(Uses Rust 1.1.0-nightly, by the way.)

[csharppattern]:    https://github.com/dotnet/roslyn/issues/206
[kelvinversioning]: http://doc.urbit.org/community/articles/martian-computing/

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
