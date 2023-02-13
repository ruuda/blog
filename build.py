#!/usr/bin/env python3

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


def main() -> None:
    generated_at, stars = load_stars()
    repos = [repo._replace(stars=stars[repo.slug]) for repo in load_repos()]
    repos.sort(key=lambda r: r.stars, reverse=True)
    print(generated_at)
    for repo in repos:
        print(repo)


if __name__ == "__main__":
    main()
