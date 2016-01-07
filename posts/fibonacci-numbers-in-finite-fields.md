---
title: Fibonacci numbers in finite fields
break: numbers in
date: 2014-07-01
math: true
synopsis: With a bit of algebra, Fibonacci numbers can be computed efficiently using only integer arithmetic.
run-in: A function
---

A function that computes the <var>n</var>-th Fibonacci number
is often one of the first things that you encounter when reading up on any language.
Here it is in C++:

```cpp
typedef std::uit64_t u64;

u64 fib(u64 n)
{
  if (n == 0) return 0;
  if (n == 1) return 1;
  return fib(n - 1) + fib(n - 2);
}
```

This implementation is simple, it reflects the mathematical definition,
and it wastes all of your CPU cycles computing the same values over and over again.
There are numerous [other ways][otherways] to compute the <var>n</var>-th Fibonacci number
<var>F<sub>n</sub></var> in a more efficient way.
A simple constant-time solution is to use the closed-form expression:

<var>F<sub>n</sub></var> = <table><td>
  <tr><td>(1 + √<span class="sqrt">5</span>)<sup><var>n</var></sup> -
          (1 - √<span class="sqrt">5</span>)<sup><var>n</var></sup></td></tr>
  <tr><td>2<sup><var>n</var></sup> √<span class="sqrt">5</span></td></tr></table>

This expression involves the square root of five,
so the naive approach is to use floating-point numbers:

```cpp
u64 fib(u64 n)
{
  double phi = 0.5 + 0.5 * sqrt(5.0);
  double psi = 0.5 - 0.5 * sqrt(5.0);
  return (pow(phi, n) - pow(psi, n)) / sqrt(5.0);
}
```

The main problem with this solution
is that a `double` cannot represent all `u64` values exactly.
On my machine, it fails from `fib(72)` onwards.
There is a [trick][trick] that you can use to improve it somewhat,
but as of `fib(79)` we run into a fundamental problem:
the correct answer cannot be represented by a `double` any more.

While I was solving an algebra exercise about the Fibonacci sequence modulo <var>p</var> the other day,
I proved that that the closed-form expression is still correct in a [finite field][finitefield] of prime order,
if the field contains a square root of five.
This can be used to give an integer-only function to compute the Fibonacci numbers,
which is what I will do in this post.

[otherways]:   http://atgcio.blogspot.nl/2013/01/computing-fibonacci.html
[trick]:       http://bosker.wordpress.com/2011/07/27/computing-fibonacci-numbers-using-binet%E2%80%99s-formula/
[finitefield]: https://en.wikipedia.org/wiki/Finite_field

Finite fields
-------------
A finite field 𝔽<sub><var>p</var></sub> of prime order <var>p</var> consists of the integers 0, 1, ..., <var>p</var> - 1.
You can do addition and multiplication,
but to ensure that the result is not too big,
take the result modulo <var>p</var>.
If <var>p</var> is prime, then for every nonzero number <var>x</var>,
there exists a number <var>x</var><sup>-1</sup> such that <var>x</var> · <var>x</var><sup>-1</sup> mod <var>p</var> = 1.
(Mathematicians omit the ‘mod <var>p</var>’, because it is clear from the context.)
You can think of this as the rational number 1/<var>x</var>,
but it is an integer in 𝔽<sub><var>p</var></sub>, not a fraction.
For example, 𝔽<sub>5</sub> consists of the integers 0 through 4.
The multiplicative inverse of 3 is 2, because 3 · 2 = 6 ≡ 1 mod 5.

How are these finite fields useful for computing Fibonacci numbers?
It turns out that the closed-form expression still works in 𝔽<sub><var>p</var></sub> with a few adaptations.
That allows us to compute the Fibonacci numbers modulo <var>p</var> with integer operations only.
I will assume that we only care about Fibonacci numbers that can be represented by a 64-bit integer.
<var>F</var><sub>93</sub> is the largest Fibonacci number still representable,
so if we can find a suitable prime <var>p</var> such that <var>F</var><sub>93</sub> < <var>p</var> < 2<sup>64</sup>,
we can compute the Fibonacci numbers in 𝔽<sub><var>p</var></sub>.

The closed-form expression explained
------------------------------------
This real number √<span class="sqrt">5</span> appears mysteriously in the closed-form expression.
Where does it come from, and how does the expression even yield integer values?
It helps to understand where the √<span class="sqrt">5</span> comes from
to understand the 𝔽<sub><var>p</var></sub> counterpart.
Define
$$ v = \sqrt{5}, \> \> \> \> φ = \frac{1 + v}{2}, \> \> \> \> ψ = \frac{1 - v}{2} $$
Note that both <var>φ</var> and <var>ψ</var> are solutions of the equation
$$ X^{2} = X + 1 $$
because
$$ \frac{(1 ± v)^{2}}{2^{2}} = \frac{1 ± 2v + v^{2}}{4} = \frac{1 ± 2v + 5}{4} = \frac{2 ± 2v}{4} + \frac{4}{4} = \frac{1 ± v}{2} + 1 $$
By multiplying both sides of the equation with <var>X</var><sup>n - 2</sup>,
we can see that <var>φ</var> and <var>ψ</var> are also solutions of the equation
$$ X^{n} = X^{n-1} + X^{n-2} $$
Now compare that to the Fibonacci relation,
<var>F<sub>n</sub></var> = <var>F</var><sub><var>n</var> - 1</sub> + <var>F</var><sub><var>n</var> - 2</sub>.
The equation satisfies the Fibonacci relation!
If <var>φ</var> and <var>ψ</var> are solutions,
a linear combination <var>a</var> + <var>b</var> is also a solution,
so if we can choose <var>a</var> and <var>b</var> such that
<var>aφ</var><sup>0</sup> + </var>bψ</var><sup>0</sup> = <var>F</var><sub>0</sub>
and <var>aφ</var><sup>1</sup> + <var>bψ</var><sup>1</sup> = <var>F</var><sub>1</sub>,
we have an expression for <var>F<sub>n</sub></var>.
Solving this yields <var>a</var> = <var>v</var><sup>-1</sup>
and <var>b</var> = - <var>v</var><sup>-1</sup>,
and that results in the expression we saw before.

This derivation makes the √<span class="sqrt">5</span> a little less mysterious,
but it shows something even more important:
the only property of <var>v</var> that we have used,
is that <var>v</var><sup>2</sup> = 5,
so if we can find a <var>v</var> that squares to five a finite field,
we can use the closed-form expression.

Magic numbers
-------------
Of course any good function needs magic numbers.
Don’t worry, we will have three.
We are looking for a number in the range 0, 1, ..., <var>p</var> - 1,
such that its square modulo <var>p</var> is five.
Because of some more advanced mathematical [reasons][quadrecipr],
such a number does not always exist.
It exists only if <var>p</var> mod 5 = ±1.
So now we need to find a prime <var>p</var>,
such that <var>F</var><sub>93</sub> < <var>p</var> < 2<sup>64</sup>,
and <var>p</var> mod 5 = 1 or <var>p</var> mod 5 = 4.
[Sage][sage] (mathematics software based on Python)
will happily provide us with a suitable prime:

```python
Pr = Primes()
p  = Pr.next(fibonacci(93))

while ((p % 5) != 1 and (p % 5) != 4):
    p = Pr.next(p)

print p
> 12200160415121876909
```

This yields a prime <var>p</var> smaller than 2<sup>64</sup>, so we’re good.
Now we know that a square root of five exists,
but how do we find it?
Sage can help us with that too:

```python
Fp   = GF(p) # Fp is the finite field of order p
five = Fp(5)
v    = sqrt(five)
print v
> 833731445503647576
```

Note that this is not the regular `sqrt` function for real numbers.
Sage knows that `five` is an element of `Fp`,
so it will search for the integer <var>v</var> such that <var>v</var><sup>2</sup> mod <var>p</var> = 5.

Finally, we need <var>v</var><sup>-1</sup>.
There are several ways to compute it,
but because it is a constant,
we can just pre-compute it with Sage:

```python
print 1/v
> 2606778372125104897
```

[quadrecipr]: https://en.wikipedia.org/wiki/Quadratic_reciprocity
[sage]:       http://sagemath.org/

Modular arithmetic
------------------
To implement the closed-form expression,
we need to do arithmetic in 𝔽<sub><var>p</var></sub>,
so all calculations are done modulo <var>p</var>.
How do we do addition?
Simply `(a + b) % p` is not going to work here,
because <var>p</var> > 2<sup>63</sup> - 1,
so `a + b` can overflow,
and that would give an incorrect result.
The solution is to check for overflow,
and correct it if it occurs:

```cpp
u64 addmod(u64 a, u64 b, u64 p)
{
  if (p - b > a) return a + b;
  else return a + b - p;
}

u64 submod(u64 a, u64 b, u64 p)
{
  if (a >= b) return a - b;
  else return p - b + a;
}
```

We also need multiplication and exponentiation.
These are a little more complicated,
and beyond the scope of this post.
Exponentiation uses the [method of successive squares][succsqr],
and the idea behind multiplication is the same.

```cpp
u64 mulmod(u64 a, u64 b, u64 p)
{
  u64 r = 0;

  while (b > 0)
  {
    if (b & 1) r = addmod(r, a, p);
    b >>= 1;
    a = addmod(a, a, p);
  }

  return r;
}

u64 powmod(u64 a, u64 e, u64 p)
{
  u64 r = 1;

  while (e > 0)
  {
    if (e & 1) r = mulmod(r, a, p);
    e >>= 1;
    a = mulmod(a, a, p);
  }

  return r;
}
```

Note that `mulmod` and `powmod` do not run in constant time:
the number of operations they perform depends on their input.
It is possible to write constant-time versions of `mulmod` and `powmod`,
but these are actually slower on average.
They will perform similarly for the worst-case input,
but the constant-time functions will take that time for _every_ input.

Finally, we need to be able to compute 2<sup>-<var>n</var></sup>,
the multiplicative inverse of 2<sup>n</sup>.
A way to do this is to use the [extended Euclidean algorithm][euclideanalg],
but because we have `powmod` already,
there is an easier way.
A [theorem][fermatltthm] in group theory tells us that for any nonzero <var>x</var> in 𝔽<sub><var>p</var></sub>,
we have <var>x</var><sup><var>p</var> - 1</sup> = 1.
This means that
$$2^{-n} = 1 · 2^{-n} = 2^{p-1} · 2^{-n} = 2^{p-1-n} $$
Because <var>n</var> will not be larger than 93,
the exponent is positive.
A positive power is something we can compute with `powmod`.

[succsqr]:      http://mathworld.wolfram.com/SuccessiveSquareMethod.html
[euclideanalg]: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
[fermatltthm]:  https://en.wikipedia.org/wiki/Fermat%27s_Little_Theorem

Putting it all together
-----------------------
Let’s first rewrite the floating-point version a bit,
so that it is easier to translat𝔽<sub><var><</var>var>p</sub>.

```cpp
u64 fib(u64 n)
{
  double v     = sqrt(5.0);
  double v_inv = 1.0 / v;

  double a        = pow(1.0 + v, n);
  double b        = pow(1.0 - v, n);
  double pow2_inv = pow(2.0, -n);

  double diff   = a - b;
  double factor = v_inv * pow2_inv;

  return diff * factor;
}
```

Now we replace `double` with `u64`,
`v` and `v_inv` with the values that we found before,
and the arithmetic operations with the corresponding modulo operations,
and it should work!
And of course, the magic numbers should be written in hexadecimal,
because magic numbers are always written in hexadecimal, right?

```cpp
u64 fib(u64 n)
{
  u64 p     = 0xa94fad42221f27ad;
  u64 v     = 0x0b92025517515f58;
  u64 v_inv = 0x242d231e3eb01b01;

  u64 a        = powmod(1 + v, n, p);
  u64 b        = powmod(p + 1 - v, n, p);
  u64 pow2_inv = powmod(2, p - 1 - n, p);

  u64 diff   = submod(a, b, p);
  u64 factor = mulmod(v_inv, pow2_inv, p);

  return mulmod(diff, factor, p);
}
```

Note again that we assumed that <var>n</var> < <var>p</var>,
but since the result is only useful for <var>n</var> < 94,
that is not really a problem.

The function does not run in constant time,
because `mulmod` and `powmod` do not run in constant time.
As I mentioned before, this is actually due to an optimisation.
If we would use a constant-time `mulmod` and `powmod`, `fib` would also be constant-time.

So there you have it,
a function that computes all Fibonacci numbers representable by a `u64` efficiently
without resorting to floating-point numbers or wider integers.
The approach in this post works for unsigned integers of any size,
provided that a suitable prime exists.
(This is the case for all common integer sizes.)
You can find my implementation of a more general function on [GitHub][github].

Discuss this post on [Reddit][reddit].

[github]: https://github.com/ruud-v-a/fibint
[reddit]: http://reddit.com/r/programming/ruudvanasseldonk.com/2014/07/01/fibonacci-numbers-in-finite-fields
