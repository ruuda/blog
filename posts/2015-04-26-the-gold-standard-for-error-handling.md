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
I am working on a [decoder][claxon] for the FLAC codec in Rust,
and a [library][hound] that handles WAV files to verify it.
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
is that you have to keep an [MSDN][msdn] tab open at all times.
Can this function throw?
Should I catch a `PathTooLongException` here?
And while the .NET framework is documented to remarkable detail,
this is rarely the case for third-party libraries or in-house code.
At a certain point, you have to assume _everything may throw_.

[msdn]: https://msdn.microsoft.com/en-us/library/gg145045.aspx
[getfullpath]: https://msdn.microsoft.com/en-us/library/system.io.path.getfullpath.aspx

Algebraic data types solve this issue in a surprisingly clean way.
They are available in many languages,
such as Haskell, Rust, Scala, and maybe even [a future version of C#][csharppattern],
but in this post I will use Rust.

Suppose we want to implement a [Kelvin versioning][kelvinversioning] scheme,
where the user can enter a new version to be released.
We first parse the input into a number,
and then check that this is a valid version to release.
If it is, we return the parsed number, otherwise we report an error.

Conceptually, this is a simple task.
If we disregard proper exception handling for a moment,
we could implement it like this in C#:

```cs
class InvalidVersionException : Exception { }

static uint CheckNextVersion(List<uint> previousVersions, string versionString)
{
  var version = uint.Parse(versionString);
  if (version <= previousVersions.Min()) return version;
  throw new InvalidVersionException();
}
```

Even though we called only two library functions,
the method may throw five different exceptions!
(Disregarding things like `OutOfMemoryException` and `ThreadAbortException`.)
This is completely implicit though,
the only way to find out is to inspect the method and consult the documentation.
To keep things under control,
we will ensure that the method throws one of these exceptions:

 - `ParseException` if the number could not be parsed into a 32-bit unsigned integer.
 - `InvalidVersionException` if it is illegal to release this version due to a previous release with a lower version numer.
 - `NewReleaseImpossibleException` if it is illegal to ever release a new version.

Properly handling all exceptions,
we get:

```cs
class ParseException : Exception { }
class InvalidVersionException : Exception { }
class NewReleaseImpossibleException : Exception { }

static uint CheckNextVersion(List<uint> previousVersions, string versionString)
{
  if (previousVersions == null) throw new ArgumentNullException("previousVersions");
  if (versionString == null) throw new ArgumentNullException("versionString");

  uint version;
  try
  {
    version = uint.Parse(versionString);
  }
  catch (FormatException) { throw new ParseException(); }
  catch (OverflowException) { throw new ParseException(); }
  // ArgumentNullException cannot occur because we validated the argument before.

  try
  {
    var min = previousVersions.Min();

    if (min == 0) throw new NewReleaseImpossibleException();
    else if (min <= version) throw new InvalidVersionException();
    else return version;
  }
  // If the list is empty there is no minimum. Initially, any version is fine.
  catch (InvalidOperationException) { return version; }
  // ArgumentNullException cannot occur because we validated the argument before.
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
it will continue to unwind stack frames until it encounters a handler, a catch.
For example, we could implement a [Kelvin versioning][kelvinversioning] scheme
like so:

```cs
string NextVersionString(uint currentVersion)
{
  return Predecessor(currentVersion).ToString();
}

void SuggestNextVersion(uint currentVersion)
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
is that it is easy to forget to handle an exception.
Without the try/catch block,
the code would compile fine,
and if there is no try/catch block,
is that because the programmer forgot to handle the exception,
or is it not handled intentionally?
In contrast, the Rust compiler refuses to compile the following function:

```rust
fn next_version_string(current_version: u32) -> String {
    use std::string::ToString;
    u32::to_string(&predecessor(current_version))
}
```

“Error: mismatched types,” it says.
“Expected `&u32`, found `&Result<u32, NoPredecessor>`.”
The compiler forces us (and now I do it again?) to handle the error here.
`to_string` can only be called if `predecessor` was succesful.
However, at this point we are not in a position to handle the error,
it must be escalated.
Rust forces us to be honest by changing the return type to `Result<String, NoPredecessor>`.
Then we can write

```rust
fn next_version_string(current_version: u32) -> Result<String, NoPredecessor> {
    use std::string::ToString;
    u32::to_string(&try!(predecessor(current_version)));
}
```

Blah

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
