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

[csharppattern]: https://github.com/dotnet/roslyn/issues/206

An example: Kelvin versioning
-----------------------------
Suppose we want to implement a [Kelvin versioning][kelvinversioning] scheme.
Kelvin versions are nonnegative integers
that _decrease_ with every release.
After 0K has been released, it is impossible to ever release a new version.

[kelvinversioning]: https://moronlab.blogspot.com/2010/01/urbit-functional-programming-from.html

Our goal is to write a function that takes a list of previously released versions
and a user-provided version string,
which then returns the parsed number or reports an error.
Conceptually, this is a simple task.
If we disregard proper exception handling for a moment,
we could implement it like this in C#:

```cs
public class InvalidVersionException : Exception { }

public static uint CheckNextVersion(IEnumerable<uint> previousVersions,
                                    string versionString)
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
we get the following:

```cs
public class ParseException : Exception { }
public class InvalidVersionException : Exception { }
public class NewReleaseImpossibleException : Exception { }

public static uint CheckNextVersion(IEnumerable<uint> previousVersions,
                                    string versionString)
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

Code analysis is still very unhappy about this.
In particular it wants us to change the exception definition oneliners
into 23-line beasts with four constructors,
but in this post I (WE?) would like to keep it brief.

Rust has an honest type system,
so we must be honest with the return type.
If a method in C# has return type `T` and throws an exception of type `E`,
the Rust return type would be `Result<T, E>`.
A result value can either be `Ok(T)` or `Err(E)`.
Let’s give it a try:

```rust
pub struct InvalidVersionError;

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, InvalidVersionError> {
    let version = version_string.parse::<u32>();
    if version <= previous_versions.iter().cloned().min() {
        Ok(version)
    } else {
        Err(InvalidVersionError)
    }
}
```

“Error,” the compiler says!
Uh oh.
“Binary operation `<=` cannot be applied to `Result<u32, ParseIntError>`.”
While a parse method returning an integer is perfectly fine in C#,
there is no place for such lies in an honest language.
Parsing can fail,
so `parse` does not return an `u32`, it returns a `Result`.
Where you can ignore the problem in C#,
Rust forces us to consider all cases.
To fix the error,
we first need a way to return either `ParseError` or `InvalidVersionError` from the function.
`Result` has only one error type,
so we must combine `ParseError` and `InvalidVersionError` into one type: a sum type,
called enum in Rust.
Let’s try again:

```rust
enum Error {
    ParseError,
    InvalidVersion
}

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, Error> {
    let version = match version_string.parse::<u32>() {
        Ok(n) => n,
        Err(_) => return Err(Error::ParseError)
    };
    if version <= previous_versions.iter().cloned().min() {
        Ok(version)
    } else {
        Err(Error::InvalidVersion)
    }
}
```

“Error” again, right after the `<=`.
Ouch.
“Mismatched types: expected `u32`, found `Option<u32>`.
The compiler is telling us that an empty set has no minimum.
With the dishonest type system of C#,
this edge case is easy to forget,
and the app could have crashed in production on an `InvalidOperationException`.
Rust prevents these bugs at compile time.
Let’s fix this,
and add a distinct error for `NewReleaseImpossible` as well.
Getting the types right will allow us to drop the `::<u32>` part from `parse` too.
Type inference in Rust is much more advanced than the simple `var` in C#:
because `version` is compared with `min` a few lines later,
the compiler can infer that `version` must be a `u32` just like `min`.
This is great when you get the types right,
but it can lead to less comprehensible compiler errors when you get it wrong.

```rust
pub enum Error {
    ParseError,
    InvalidVersion,
    NewReleaseImpossible
}

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, Error> {
    let version = match version_string.parse() {
        Ok(n) => n,
        Err(_) => return Err(Error::ParseError)
    };
    if let Some(min) = previous_versions.iter().cloned().min() {
        if version <= min {
            Ok(version)
        } else if min == 0 {
            Err(Error::NewReleaseImpossible)
        } else {
            Err(Error::InvalidVersion)
        }
    } else {
        Ok(version) // Initially, any version is fine.
    }
}
```

This version compiles,
and I would say it is about as verbose as the C# version apart from the lack of null checks.
(Rust’s type sytem is honest. It does not have null.)
The difference between C# and Rust here is not so much in the version with proper error handling,
it is in the version without proper error handling:
the Rust compiler refused to compile it,
wherease the C# version crashed at runtime.

Error propagation
-----------------
The comparison above is not entirely fair.
In the example, we handled _all_ exceptions.
The true power of exceptions though,
lies in _not_ handling them.
They automatically propagate up the call stack,
until a handler is encountered.

Suppose that instead of throwing our custom `ParseException` type,
we would be fine with the method throwing `FormatException` or `OverflowException`.
Then this:

```cs
uint version;
try
{
  version = uint.Parse(versionString);
}
catch (FormatException) { throw new ParseException(); }
catch (OverflowException) { throw new ParseException(); }
```

would simplify to what we started with:

```
var version = uint.Parse(versionString);
```

How would we go about that in Rust?
If our function returned `Result<u32, ParseIntError>`,
we could just return the error,
but this is not the case.
We need a way to convert a `ParseIntError` into `Error::ParseError` automatically.
The way to do this, is by implementing `From<ParseIntError>` for `Error`:

```rust
use std::num::ParseIntError;

impl From<ParseIntError> for Error {
   fn from(_: ParseIntError) -> Error {
       Error::ParseError
   }
}
```

By using `_` as argument, we throw away the error data.
It is possible to make `Error::ParseError` wrap the original error instead,
but in this post I (WE?) will keep it simple.

Instead of this:

```rust
let version = match version_string.parse() {
    Ok(n) => n,
    Err(_) => return Err(Error::ParseError)
};
```

we can now write this:

```rust
let version = try!(version_string.parse());
```

It is almost as concise as the C# version,
but the error handling is explicit here.
The `try!` macro expands to the same match statement that we wrote manually before,
calling `From::from` on the error value to convert it into the desired type.
From the `try!` macro it is immediately clear that error handling is going on here,
and that control flow may exit the function at this point.
In C# this is implicit.

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
