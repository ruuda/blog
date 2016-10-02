---
title: Working on a virtualenv without magic
date: 2016-10-02
minutes: 3
synopsis: The standard way of activating a Python virtualenv has a number of issues. Here is a simpler, understandable method.
run-in: To keep installed dependencies
---

To keep installed dependencies nice and isolated,
Python programmers run their code inside a virtualenv.
Unfortunately virtualenvs can be a bit hairy.
They require you to *source* an 80-line shell script (eww),
and you need to locate the activate script manually inside the virutalenv.
Fortunately, there is [Virtualenvwrapper][venvwrapper].
After all, any problem in computer science can be solved with another layer of indirection.
Among other things, it provides a handy `workon` command to activate a virtualenv.
But do we really *need* any of this?
And what is actually going on?

I don’t like tools that I don’t understand.
That is not to say that I am wary of tools that hide complexity,
as long as I know *what* they are hiding from me.
Frustrated by the virtualenv magic,
I decided to investigate.
And surely enough, I was not the only one who felt like this.
Michael F. Lamb has [a great writeup][inve-gist] about what virtualenv actually does,
and how to do that in a better way.
Instead of repeating that,
I’ll share the solution that I derived from it here.

[venvwrapper]: https://virtualenvwrapper.readthedocs.io/en/latest/index.html
[inve-gist]:   https://gist.github.com/datagrok/2199506

A simpler workon
----------------

It turns out that the only thing required to get the virtualenv working,
is to set and unset a few environment variables.
That shouldn’t be too hard.
The next thing to do is to start a new shell with this environment,
so you can get out of the virtualenv simply with `exit` -- not some ad-hoc `deactivate` command.
I put this all together in a `workon` function defined in my shell startup script:

```sh
# A function to activate a virtualenv without sourcing madness.
# Based on https://gist.github.com/datagrok/2199506.
function workon {
  export VIRTUAL_ENV="$HOME/env/$1"
  export PATH="$VIRTUAL_ENV/bin:$PATH"
  unset PYTHON_HOME
  $SHELL
}
```

It behaves similar to Virtualenvwrapper:
`workon foo` will activate the `~/env/foo` virtualenv.
Note that I am storing my virtualenvs in `~/env`.

Adding tab completion
---------------------

I quickly grew tired of not having tab completion for my custom `workon`,
so I added the following Zsh completion definition right after the function:

```sh
# Autocomplete the "workon" command with directories in ~/env.
compdef '_path_files -/ -g "$HOME/env/*" -W "$HOME/env/"' workon
```

Here `_path_files` activates filename completion.
The `-/` flag specifies that only directories should be completed,
and `-g` specifies that only directories matching the given pattern should be suggested.
Finally, the `-W` flag indicates that the prefix `$HOME/env/`
should be stripped from the suggested paths.

Decorating the prompt
---------------------

One thing the `activate` script does in addition to setting environment variables,
is putting the name of the virtualenv in the promt.
In the process it messes up the newlines in my prompt,
and I can’t stand the lack of a space after the virtualenv name.
I am glad to have gotten rid of this “feature”,
although occasionally it can be useful to know whether you are inside a virtualenv.
Putting it in the prompt is a nice indicator.
Fortunately that is not hard to do at all:

```sh
if [ -n "$VIRTUAL_ENV" ]; then
  VENV_PROMPT=" $(basename $VIRTUAL_ENV)"
else
  VENV_PROMPT=""
fi
```

Inside a virtualenv, `$VENV_PROMPT` will be the name of the virtualenv, prefixed by a space.
Outside of a virtualenv it will be empty.
This variable can then be used in the prompt however you like.
