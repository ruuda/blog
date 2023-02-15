#!/usr/bin/env python3

# Copyright 2023 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from datetime import datetime
from typing import Dict, Iterable, List, NamedTuple, Tuple

import json
import toml


class Repo(NamedTuple):
    title: str
    slug: str
    description: str
    stars: int


def load_repos() -> Iterable[Repo]:
    with open("repos.toml", "r", encoding="utf-8") as f:
        for title, properties in toml.load(f).items():
            yield Repo(title, **properties, stars=0)


def load_stars() -> Tuple[datetime, Dict[str, int]]:
    with open("stars.json", "r", encoding="utf-8") as f:
        data = json.load(f)
        generated_at = datetime.fromisoformat(data["generated_at"])
        assert generated_at.tzinfo is not None
        stars: Dict[str, int] = data["stars"]
        return generated_at, stars


def render_project_card(repo: Repo) -> str:
    return f"""
    <div class="projectlink">
      <h3><a href="{repo.slug}/">{repo.title}</a></h3>
      <p class="desc">{repo.description}</p>
      <p class="src"><span class="stars">★ {repo.stars}</span>
      ⋅ <a href="https://github.com/ruuda/{repo.slug}">GitHub</a>
      ⋅ <a href="https://codeberg.org/ruuda/{repo.slug}">Codeberg</a>
      </p>
    </div>
    """


def render_template(variables: Dict[str, str]) -> str:
    with open("index.html", "r", encoding="utf-8") as f:
        output = f.read()
        for needle, replacement in variables.items():
            output = output.replace("{{" + needle + "}}", replacement)

    return output


def main() -> None:
    generated_at, stars = load_stars()
    repos = [repo._replace(stars=stars[repo.slug]) for repo in load_repos()]
    repos.sort(key=lambda r: r.stars, reverse=True)
    variables = {
        "generated_at": generated_at.strftime("%B %Y"),
        "projects": "".join(render_project_card(repo) for repo in repos),
    }
    print(render_template(variables))


if __name__ == "__main__":
    main()
