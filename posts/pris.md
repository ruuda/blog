---
title: A language for designing slides
break: language for
date: 2017-02-19
synopsis: I made something. To do.
run-in: It is my opinion
---

It is my opinion that great slides are designed.
Applying a fancy template to a dull set of bullet points has never been easier than today,
and good slides need not be beautiful.
But great slides -- great slides are *designed*.
As a programmer with a secret love for typography,
and a slight control freak,
none of the tools that I have used allow me to design slides in the way I would like.
So I built my own.

The status quo
--------------

Design requires absolute control of where graphics are placed.
This level of control has traditionally been reserved for graphical editors
such as Powerpoint and Illustrator.
Although these editors allow full control,
they come with a few serious drawbacks.
Their binary formats are not friendly to source control, for instance.
But the real problem is that **operations in a visual editor do not compose**.
Anyone who has ever tried to animate a diagram in Powerpoint
should understand what I mean here.
The way it is done, is rougly like this:

1. Draw the most complete version of the diagram.
2. Copy it onto a few slides.
3. Edit every copy to create the individual frames.

And if after that you want to change the colour of a piece of text
that occurs on all slides,
or move it a bit ...
then well, you are stuck.
If the change is minor,
you might try to do exactly the same edit on all slides.
If the slides do not differ much from the base diagram,
deleting them all and starting over with fresh copies might be the best option.
Obviously neither of these are desirable.

The solution then,
is to not edit the diagram directly,
but to generate it from some kind of specification.
TikZ in a Beamer presentation is a good example of this,
and it solves the diagram problem well.
I have used it with success for presentations about Git,
with graph drawings to explain how operations manipulate the DAG.
The Fontspec package gives me full typographic control,
and TeX files are friendly to source control.
Getting to an initial version of a drawing is more work
due to the longer feedback loop,
but that is the tradeoff for being able to make edits later on.
But although Beamer satisfies many of my needs,
it is fundametally not the tool that I wish for.

Beamer is a LaTeX package,
and as such it does not offer precise control over lay-out.
The entire point of TeX is that it takes care of lay-out for you.
For long documents full of text this makes sense.
But for slides with little text, I want control.
One could do everything in an embedded TikZ drawing,
but this is tedious.
TikZ and similar systems such as Metapost are great for complex drawings,
but not ergonomic for typographic design.
Although the basic drawing primitives compose,
**parametrising and reusing graphics is difficult**.
Macros are awkward and in many ways limited.

Different fundamentals
----------------------

The problem with TikZ and similar systems is twofold.
Firstly, they are too domain-specific to make automating things viable.
Macro definitions are no substitute for variables or functions,
beause they deal with tokens, not values.
It is like C without functions, only preprocessor macros.
Secondly, all of the drawing DSL<!---->s that I have used manipulate a canvas directly.
This means that the only available mechanism for reuse is necessarily procedural.
Draw calls might be grouped in a procedure and parametrised over some inputs,
but this approach is fundamentally limited.

Let me demonstrate this limitation with an example.
Say I have a procedure that draws a rectangle
with its top-left corner at a given coordinate.
How do I draw it with its centre at a given coordinate?
I would have to know its size beforehand,
and offset the input coordinates appropriately.
For a rectangle this is doable,
but for more complex shapes,
computing the size beforehand quickly becomes impractical.

The issue with the procedural approach
is that once graphics are drawn,
they are set in stone.
What I would like instead,
is a system where graphics are first-class.
Where they can be inspected and manipulated *after* being drawn.
And I want full scripting with proper functions.

I started writing down things in a hypothetical language,
to get a clear picture of what my ideal tool would look like.
For a while I investigated building an embedded DSL
in a general-purpose scripting language like Python or Lua,
but it quickly became clear to me
that these were going to be too noisy to be practical.
And so I started working on an interpreter for that hypothetical language.
Today it is no longer hypothetical.

Pris
----

So how does Pris
-- which is what I called my DSL
-- solve the rectangle problem?
Drawing a rectangle with its top-left corner at a given location
is not so different from other tools:

    {
      at (1em, 1em) put fill_rectangle((1em, 1em))
    }

The challenge now is to put the rectangle
with its centre at `(1em, 1em)`,
without using the knowledge that its sides are `1em` long.
In Pris, that is done as follows:

    {
      rect = fill_rectangle((1em, 1em))
      at (1em, 1em) - rect.size * 0.5 put rect
    }

This example shows that graphics are *first class*:
they can be assigned to variables,
and their size can be queried.
Nothing is drawn until something is `put` on the canvas.
Coordinate arithmetic is also supported out of the box.

To take this one step further,
we might extract the alignment logic into a function:

    center = function(frame)
    {
      at frame.size * -0.5 put frame
    }

    {
      at (1em, 1em) put center(fill_rectangle((1em, 1em)))
    }

This time there is a `put` inside the function,
but it does not draw anything on the main canvas like a procedure would do.
Functions in Pris are pure, free of side effects.
The `put` only places a graphic locally in the scope of the function.
The result of calling the function is itself a new graphic,
which can be placed on the main canvas (demarked by bare braces).
This example also shows that functions are first-class values.

Small caveat: the `center` function will do the wrong thing
if the bounding box extends to the left of, or above `(0,` `0)`.
The variables for doing proper alignment in that case are not yet exposed.

Progress report
---------------

An exprerimental implementation of Pris exists.
It is free software,
[available on GitHub](https://github.com/ruuda/pris#readme).
The current feature set is limited,
and I intend to make changes to the syntax and exposed functions still.
Nonetheless, I have used it to do one set of slides so far.
This helped me prioritise features,
and to sort out what works and what doesn’t.
The number of implemented primitives is small,
but placing SVG graphics and rendering text is supported,
which for many things is sufficent.
I plan to continue in the same way:
implement features as I need them,
and make things more ergonomic when they start to become unwieldy.

If Pris seems useful to you,
then please go ahead and try it.
I will have to invest more in diagnostics and documentation;
for now there are only [the examples](https://github.com/ruuda/pris/tree/master/examples).
There are no binaries available yet,
but the build process is straightforward.
If you get stuck somewhere then feel free to [reach out](/contact) to me.
And if you have any insights, please do share them.
