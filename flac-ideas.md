- Many decoders crash when encountering corrupted or invalid files.
- Rust is a safe language, decoding should never be able to crash a program;
  errors can be reported properly via `Result`, and due to safety, there should
  not be random crashes.
- Starting out with a video codec is probably too hard.
- I like flac; I use it daily, and there is a clear specification and reference
  implementation.
- => I will write a flac decoder in Rust.
- Programming with `Result` and `try!` is extremely nice. You get this monadic
  style where you focus on the “happy path” (as Erik Meijer likes to call it),
  though the “sad path” is clear, explicit and type-safe. You need not worry
  about exceptions like in C#; it is impossible to misuse this.
- I am confronted with things that may fail almost everywhere, and I am forced
  to think about it and make a decision everywhere. (No unexpected failures at
  runtime though, unless you’d use `unwrap` everywhere without being careful.)
  However, in almost all cases, `try!` is a great solution.
- Going pretty well up to this point (implementing metadata reading, streaminfo
  works). A sudden realisation: I think the API will be quite nice, but suppose
  I was writing this in idiomatic C# … In Rust data structures are owned and
  immutable by default, it is as simple as defining the struct. In C#, how I
  would do it is to have private readonly (misleading keyword btw) fields, and
  then have public get-only properties. Then you would need a constructor, and
  have the constructor throw an `ArgumentNullException` if any argument is
  missing; next you would need to do validation and throw maybe a different
  kind of exception, and then finally you can set the private fields. So far
  for a structure that holds immutable data. No null and no shared writes make
  such a big difference, but I got spoiled quickly. Just look back at C# now,
  and think how much boilerplate you would need to write the same program.
      A different kind of structure is one that contains a resource. For
  example, the `MetadataBlockReader` that borrows a `Reader`. Rust statically
  ensures that while you are enumerating metadata, nothing else can read from
  the same stream. In C# this is impossible to guarantee, if you get a stream
  from somewhere, the place that you got it from might still be using it. Also,
  do you have to dispose of it afterwards, or is it going to be used again?
  When you do need to implement dispose, then you get this awful pattern; and
  you need to call `Dispose` on the members manually (a step backwards from
  C++), all boilerplate that is unnecessary in Rust. Furthermore, Rust
  statically ensures that the resources are released exactly once at the right
  time. So instead of having to implement `Dispose` manually, you add a
  lifetime and everything is done for you, and as a bonus, you get guaranteed
  unique access to the stream. Isn’t that awesome?
- This project is pretty interesting again; Robigo Luculenta had little IO and
  no real chance of failure outside of IO. Decoding flac is heavy on IO and it
  can fail at almost any point, so it is good to get to see this side of Rust.
- There is a disadvantage to `Result` though: there is no context by default,
  so by the time you actually handle the error, you might have no idea where
  it originated from. There is no attached call stack (by default). Exceptions
  suffer from the same problem in theory, but in C# they have call stacks, and
  when they are synchronous, you can break on them.
- I do miss a debugger, but not that much.
- This monadic `try!` is awesome, because you can use it anywhere.
  Conceptually, it is no different from C#'s `await`. Would an `await!` macro
  be possible for Rust? C# has a lot of compiler magic going on to do it,
  can Rust do it with macros?
