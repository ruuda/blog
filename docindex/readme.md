# Documentation Index

This directory contains the script and template that generate the index page for
<https://docs.ruuda.nl>. That documentation site is otherwise not so related to
my main site and the blog generator, but I wanted to put the code somewhere, so
I might as well put it here.

 * `build.py` writes the html to stdout, based on `repos.toml` and `stars.json`.
 * `get_stars.py` refreshes `stars.json` based on `repos.toml`.

Both scripts have no dependencies other than Python ≥3.11.

## License

The scripts are licensed under the GPL v3 like all of the other code in this
repository. The index page stylesheet is licensed Apache 2.0 like [the
Kilsbergen theme](https://github.com/ruuda/kilsbergen) itself.
