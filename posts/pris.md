---
title: A language for designing slides
break: language for
date: 2017-02-19
synopsis: I made something. To do.
run-in: It is my opinion
---

It is my opinion that great slides are designed.
Applying a nice template to a dull set of bullet points has never been easier than today.
And good slides need not be beautiful.
But great slides, are *designed*.
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
This is a superficial complaint however.
The real problem is that **operations in a visual editor do not compose**.
Anyone who has ever tried to animate a diagram in Powerpoint
should understand what I mean here.
The way it is done, is rougly like this:

1. Draw the most complete version of the diagram.
2. Copy it onto a few slides.
3. Edit every copy to create the individual frames.

And if after that you want to change the color of a piece of text
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
I have used them with success for presentations about Git,
with intricate graph drawings to explain how operations manipulate the DAG.
The Fontspec package gives me full typographic control,
and TeX files are friendly to source control.
Getting to an initial version of a drawing is more work with TikZ
due to the longer feedback loop,
but that is the tradeoff for being able to make edits later on.
But although Beamer satisfies many of my needs,
it is fundametally not the tool that I wish for.

Old stuff
---------

Lately I’ve been pondering a lot on what makes a good presentation,
and especially how to make good slides.
Most slides I see range from pretty bad to okay-ish.
Some are good.
Few are great.
A good presentation can have pretty standard slides.
Bullet points are not necessarily bad.
(How do I say this?)
Great slides are *designed*.

So when making slides, what do I want?
I want to be in control.
I want full control of where graphics and text are placed,
and over the typographic details.
Typesetting large paragraphs of text can and should be automated,
but good slides contain very little text.
Automatic line breaking is not actually that useful here.
Being a programmer,
I would like a system that plays nice with source control.
And finally,
I want to be able to tweak my slides with little effort.

Available tools
---------------

There is a plethora of tools for making slides.
I grew up with Powerpoint.
I’ve only recently come to realise that a graphical tool like Powerpoint
is actually very close to what I would want.


A language for designing slides
-------------------------------
