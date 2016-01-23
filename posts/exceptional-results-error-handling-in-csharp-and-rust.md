---
title: Exceptional results: error handling with C# and Rust
header: Exceptional results
subheader: Error handling with C# and Rust
date: 2015-06-17
synopsis: Monadic error handling is an alternative to exception-based systems that can prevent many programming errors at compile time.
run-in: When I first
---

When I first learned about monadic error handling, I was delighted.
Error handling is something that every robust program has to deal with.
There are various approaches to it,
but often a programming language forces its particular paradigm on you.
For most mainstream languages, this means exceptions.
Monadic error handling is a less widespread alternative.
Algebraic data types allow for the return type of a function to encode an error type
in addition to a “success” type,
and monads are the tool to combine such functions with ease.

In C#, exceptions are the standard way of doing error handling.
I have written plenty of C# code that deals with exceptions,
and it is a pain to do it correctly.
Apart from null, it might very well be the biggest design mistake of the language.
I was delighted when I learned about [monadic error handling][eithermonad],
with the `Either` type in Haskell.
Obviously this was the right approach to error handling, elegant and effective.

[eithermonad]: http://www.learnyouahaskell.com/for-a-few-monads-more

Along came Rust, and I immediately fell in love with it.
It appeared to do everything right that C# did wrong.
Not only did it feature the absence of null (an entire class of problems ... gone!),
it also had unrecoverable failures (now called panics),
and monadic error handling for the cases where errors are not exceptional.

Fast forward one year.
[Claxon][claxon] -- my pure-Rust decoder for the FLAC codec --
decoded its entire test suite correctly for the first time a week ago.
With [Hound][hound], a Rust library for handling WAV files,
I could verify the output against the reference decoder.
When dealing with these binary formats,
things can fail at several levels:
apart from IO errors,
there are the issues of ill-formed data and incorrect usage of the library
(e.g. writing an odd number of samples to a stereo stream).
So, I got to do some error handling in Rust.

Aside from a few toy programs in Scala and Haskell,
I had not used monadic error handling before.
Claxon and Hound are my first serious projects
where I got to see how my expectations would hold up.
Mostly, monadic error handling is a breeze,
but it has its downsides too.

[claxon]: https://github.com/ruud-v-a/claxon
[hound]:  https://github.com/ruud-v-a/hound

Liar, liar!
-----------
The issue with exceptions, in my opinion,
is that they are an escape hatch in the type system.
As Erik Meijer likes to call it, they make a type system [dishonest][meijer2008].
At least, in C# they do.
Some languages in theory implement exceptions in a more honest manner,
but they suffer from the same problem in practice.

[meijer2008]: https://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-Functional-Programming

The consequence, when you want to write robust software in C#,
is that you have to keep an [MSDN][msdn] tab open at all times.
Can this method throw?
Should I catch a `PathTooLongException` here?
And while the .NET framework is documented to remarkable detail,
this is rarely the case for third-party libraries or in-house code.
At the bottom of the call stack, you just have to assume _everything may throw_.
Catch-all handlers may silence the problem,
but they are no true solution,
and cause even worse problems of their own.

[msdn]: https://msdn.microsoft.com/en-us/library/gg145045.aspx
[getfullpath]: https://msdn.microsoft.com/en-us/library/system.io.path.getfullpath.aspx

Algebraic data types solve the issue in a surprisingly clean way.
They are available in languages
such as Haskell, Rust, Scala, and maybe even [a future version of C#][csharppattern],
but in this post I will use Rust.

[csharppattern]: https://github.com/dotnet/roslyn/issues/206

An example: Kelvin versioning
-----------------------------
Suppose we want to implement a [Kelvin versioning][kelvinversioning] scheme.
Kelvin versions are nonnegative integers
that _decrease_ with every release.
After zero Kelvin has been released, it is impossible to ever release a new version.

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
  if (version < previousVersions.Min()) return version;
  throw new InvalidVersionException();
}
```

Even though we called only two library functions,
the method may throw no less than five different exceptions!
(Disregarding things like `OutOfMemoryException` and `ThreadAbortException`.)
This is completely implicit though.
The only way to find out is to inspect the method and consult the documentation.
To keep things under control,
we will ensure that the method throws one of these exceptions:

 - `ParseException` if the number could not be parsed into a 32-bit unsigned integer.
 - `InvalidVersionException` if it is illegal to release this version
   due to a previous release with a lower or equal version number.
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
    if (min <= version) throw new InvalidVersionException();

    return version;
  }
  // If the list is empty there is no minimum. Initially, any version is fine.
  catch (InvalidOperationException) { return version; }
  // ArgumentNullException cannot occur because we validated the argument before.
}
```

Code analysis is still very unhappy about this.
In particular it wants us to change the exception definition oneliners
into 23-line beasts with four constructors,
but in this post I would like to keep it brief.

Rust has an honest type system,
so we must be honest with the return type.
If a method in C# has return type `T` and throws an exception of type `E`,
the Rust return type would be `Result<T, E>`.
A result value can either be `Ok(T)` or `Err(E)`.
Let’s give that a try:

```rust
pub struct InvalidVersionError;

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, InvalidVersionError> {
    let version = version_string.parse::<u32>();
    if version < previous_versions.iter().cloned().min() {
        Ok(version)
    } else {
        Err(InvalidVersionError)
    }
}
```

“Error,” the compiler says!
Uh oh.
“Binary operation `<` cannot be applied to `Result<u32, ParseIntError>`.”
While a parse method returning an integer is perfectly fine in C#,
there is no place for such blatant lies in an honest language.
Parsing can fail,
so `parse` does not return a `u32`, it returns a `Result`.
Where you can ignore the problem in C#,
Rust forces us to consider all cases.

To fix the error,
we first need a way to return either `ParseFailed` or `InvalidVersion` from the function.
`Result` has only one error type,
so we need a single type that can be either of the two errors: an enum.
Let’s try again:

```rust
pub enum KelvinError {
    ParseFailed,
    InvalidVersion
}

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, KelvinError> {
    let version = match version_string.parse::<u32>() {
        Ok(n) => n,
        Err(_) => return Err(KelvinError::ParseFailed)
    };
    if version < previous_versions.iter().cloned().min() {
        Ok(version)
    } else {
        Err(KelvinError::InvalidVersion)
    }
}
```

“Error” again, right after the `<`.
Ouch.
“Mismatched types: expected `u32`, found `Option<u32>`.”
The compiler is telling us that an empty set has no minimum.
With the dishonest type system of C#,
this edge case is easy to forget,
and the app could have crashed in production on an `InvalidOperationException`.
Rust prevents these bugs at compile time.

While we fix this,
let’s add a distinct error for `NewReleaseImpossible` as well.
Additionally, getting the types right will allow us to drop the `::<u32>` part from `parse`.
Type inference in Rust is much more advanced than the simple `var` in C#.
Because `version` is compared with `min` a few lines later,
the compiler is able to infer that `version` must be a `u32` just like `min`.
This is great when you get the types right,
but it can lead to less comprehensible compiler errors when you get them wrong.

```rust
pub enum KelvinError {
    ParseFailed,
    InvalidVersion,
    NewReleaseImpossible
}

pub fn check_next_version(previous_versions: &[u32],
                          version_string: &str)
                          -> Result<u32, KelvinError> {
    let version = match version_string.parse() {
        Ok(n) => n,
        Err(_) => return Err(KelvinError::ParseFailed)
    };
    if let Some(min) = previous_versions.iter().cloned().min() {
        if version < min {
            Ok(version)
        } else if min == 0 {
            Err(KelvinError::NewReleaseImpossible)
        } else {
            Err(KelvinError::InvalidVersion)
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
it is in the version without:
the Rust compiler refused to compile it,
whereas the C# version crashed at runtime.

Error propagation
-----------------
The comparison above is not entirely fair.
In the example, we handled _all_ exceptions.
The true power of exceptions though,
lies in _not_ handling them.
They automatically propagate down the call stack,
until a handler is encountered.

Suppose that instead of throwing our custom `ParseException`,
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

```cs
var version = uint.Parse(versionString);
```

How would we go about that in Rust?
If our function returned `Result<u32, ParseIntError>`,
we could just return the error,
but this is not the case.
We need a way to convert a `ParseIntError` into a `KelvinError` automatically.
The way to do this, is by implementing `From` for `KelvinError`:

```rust
use std::num::ParseIntError;

impl From<ParseIntError> for KelvinError {
   fn from(_: ParseIntError) -> KelvinError {
       KelvinError::ParseFailed
   }
}
```

We ignore the actual error data, as indicated by the `_`.
It is possible to make `ParseFailed` wrap the original error instead,
but in this post I will keep it simple.
Instead of this:

```rust
let version = match version_string.parse() {
    Ok(n) => n,
    Err(_) => return Err(KelvinError::ParseFailed)
};
```

we can now write this:

```rust
let version = try!(version_string.parse());
```

It is almost as concise as the C# version,
but error handling is explicit here.
The `try!` macro expands to the same match statement that we wrote manually before,
calling `From::from` on the error value to convert it into the desired type.
From the `try!` macro it is immediately clear that error handling is going on here,
and that control flow may exit the function at this point.
In C# this is implicit.

If the errors that may occur in a method all have the same type,
or when `From` is implemented for the error type,
error propagation with `try!` is as simple as with exceptions.
For example, concatenating two files is as simple as this:

```rust
use std::fs::File;
use std::io;
use std::io::Read;

fn cat(first: &mut File, second: &mut File) -> Result<Vec<u8>, io::Error> {
    let mut buffer = Vec::new();
    try!(first.read_to_end(&mut buffer));
    try!(second.read_to_end(&mut buffer));
    Ok(buffer)
}
```

Apart from `try!`,
there are also methods like `map` and `and_then` (the monad ‘bind’ operation)
for the more functionally inclined.

The downside
------------
Monadic error handling seems too good to be true.
With algebraic data types, the errors that a function can return
are known and checked at compile time.
It allows for error handling to be expressed in an exceptionally elegant way.
Where is the tradeoff?
When writing Claxon, I ran into one downside in particular.
You won’t see it in the above examples,
because it is not there after the program has been written.
It is an issue with debuggability.

The error type of a `Result` carries no more information than strictly necessary.
On the one hand this makes them very efficient,
but on the other hand they lack critical debugging context.
In particular, a typical error does not contain a site of origin,
no call stack,
nor a traceroute to the current call site.
Take the `cat` function above, for instance.
Suppose it returns `Err(err)`, where `err` is an `io::Error`.
Its `description()` contains all information about what went wrong reading,
but it does not tell you _which_ of the two reads failed.
Combining functions that may fail is easy with `try!`,
but disentangling them after something has failed is a pain.

Furthermore,
Visual Studio has outstanding tooling for exceptions.
You can select specifically to break on some exceptions when thrown,
or when unhandled by user code.
.NET exceptions contain a full stack trace and information about the target site.
All of this is currently not possible with Rust’s `Result`,
though I think this is not a fundamental impossibility.
I can imagine the compiler inserting a breakpoint when an `Err` is constructed,
and maybe an error type can carry
a list of source locations when compiled in debug mode.
The `try!` macro could push its location in the source file before it propagates the error.
This would attach traceroute-like information to an error in debug mode,
and the release build would be every bit as efficient.
Nevertheless, Rust is a young language,
and I am not aware of any features like these today.

Conclusion
----------
Exceptions and results are two ways to do error handling,
both with their advantages and disadvantages.
The big downside of exceptions in C#,
is that handling them
can be -- and _will be_ -- ignored or forgotten.
This leads to crashes,
crashes that may make it into the field.
A proper type system like Rust’s can prevent these errors at compile time.
There is room for improvement on the debugging side,
but nonetheless monadic error handling with algebraic data types is a powerful tool.

There are a lot of sides to the subject that I did not touch upon in this post.
Performance is an issue that deserves a post in its own right,
and the distinction between recoverable and unrecoverable errors
(which C# does not make but Rust does) is an interesting issue as well.
Niko Matsakis recently wrote about [enums versus virtual structs][enumvirt],
which relates to exception hierarchies,
a way to control the granularity of error handling.
Finally, I touched only superficially on the `Result` type in Rust,
and I did not talk about the `Error` trait at all.
If you like to know more about error handling in Rust,
Andrew Gallant wrote a [fantastic blog post][burntsushi] a month ago
that dives deep into all of the details.
If you had not heard of Rust before but this post got you excited,
the [book][book] is a great place to start.
Rust has an amazing community that is very welcoming to new members.

[enumvirt]:   http://smallcultfollowing.com/babysteps/blog/2015/05/29/classes-strike-back/
[burntsushi]: http://blog.burntsushi.net/rust-error-handling/
[book]:       https://doc.rust-lang.org/stable/book/

Rust 1.0.0 and C# 5 were used in this post.
