---
title: C# await is the Haskell do notation
date: 2013-08-20 21:30
---

As a follow-up to the [task monad](/2013/05/01/the-task-monad-in-csharp),
let’s make a comparison between the new `async` and `await` syntax in C# 5,
and the do notation in Haskell. Two constructs that might seem unrelated at
first, allow code to be written in a form that is _exactly_ the same.

<!--more-->

The do notation
---------------
Haskellers figured out that monads could be used to do IO a long time ago, but
it was clumsy. Consider the classic example of asking a user for a name and
printing it back. In Haskell, we would start with:

```haskell
askFirstName = putStrLn "What is your first name?" >> getLine
askLastName  = putStrLn "What is your last name?"  >> getLine
sayHi firstName lastName = putStrLn $ "Hello " ++ firstName ++
                           " " ++ lastName ++ "."
```

For those new to Haskell, the `>>` operator here sequences two operations, so
that the message is printed before a name is read. The `$` is like an opening
parenthesis with an implicit closing parenthesis, and `++` concatenates strings.

In normal, synchronous C#, this could be written as follows:

```cs
string AskFirstName()
{
  Console.WriteLine("What is your first name?");
  return Console.ReadLine();
}

string AskLastName()
{
  Console.WriteLine("What is your last name?");
  return Console.ReadLine();
}

void SayHi(string firstName, string lastName)
{
  Console.WriteLine("Hello {0} {1}.", firstName, lastName);
}
```

Now these functions and methods need to be composed to create the program.
Without the do notation, the Haskell version is acceptable, but misleading.
The indentation that I use here is allowed, but it hides the fact that
lambdas are nested deeper and deeper.

```haskell
main = askFirstName
       >>= \firstName -> askLastName
       >>= \lastName -> sayHi firstName lastName
```

The `>>=` is the monadic ‘bind’ operator, which in this case ensures that its
left hand side (the previous line) is executed before its right hand side. In
Haskell, the `\` introduces a lambda function, with arguments before the `->`,
and the body after it. The C# analog looks more comprehensible:

```cs
void Main()
{
  string firstName = AskFirstName();
  string lastName  = AskLastName();
  SayHi(firstName, lastName);
}
```

Expressing this kind of sequential code imperatively just makes more sense,
so the Haskell people came up with the _do notation_. The do notation allows
one to write the following:

```haskell
main = do
       firstName <- askFirstName
       lastName  <- askLastName
       sayHi firstName lastName
```

As you can see, this resembles an imperative programming style. The compiler
translates it into the first version. With monads and the do notation, Haskell
has powerful tools to write [pure](https://en.wikipedia.org/wiki/Pure_function)
progams in a convenient manner.

Re-discovering the wheel
------------------------
So far, the C# version has been synchronous. What if `Console` offered
asynchronous methods? This might seem far-fetched, but if you replace our simple
example with a network socket in a high-performance web service, it makes
a lot of sense to use asynchronous tasks if the work is IO-bound. Let’s
pretend that we have `AskFirstNameAsync` which asks for a name and reads it from
the console in a non-blocking way. It should return a `Task<string>`. Similarly,
we need `AskLastNameAsync`, and `SayHiAsync` which returns a `Task`. If we use
some of the `Then` [methods](https://blogs.msdn.com/b/pfxteam/archive/2010/11/21/10094564.aspx)
that I have talked about before, the async program could be written as follows:

```cs
Task MainAsync()
{
  return AskFirstNameAsync()
  .Then(firstName => AskLastNameAsync()
  .Then(lastName => SayHiAsync(firstName, lastName)));
}
```

Except for syntactic differences, this version has _exactly_ the same form as
the first Haskell attempt. (Note that the indentation is misleading here as well.)
Just like the Haskellers, the C# people recognised that this is a not the best
way to write non-blocking code, so they introduced `await`. With `await`, the
code can be re-written as follows:

```cs
async Task MainAsync()
{
  string firstName = await AskFirstNameAsync();
  string lastName = await AskLastNameAsync();
  await SayHiAsync(firstName, lastName);
}
```

Better indeed, but the remarkable thing is — this code has _exactly_ the same
structure as the Haskell version with do notation! I do not know whether the C#
designers were inspired by the do notation, but it is striking that the
constructs are so similar, both in the clumsy syntax, as well as the improved
syntax. Also note the similarity between asynchronous C# and Haskell: Haskell
code naturally has this asynchronous form, and it supports non-blocking IO with
minimal effort.
