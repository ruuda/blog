#!/usr/bin/env python3

# Copyright 2023 Ruud van Asseldonk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3. See
# the licence file in the root of the repository.

from datetime import datetime, timezone
from http.client import HTTPSConnection
from typing import Dict

import json
import urllib.request

from build import load_repos


def count_stars_forgejo(client: HTTPSConnection, owner: str, repo: str) -> int:
    n = 0
    page = 0

    while True:
        page += 1
        client.request(
            "GET",
            f"/api/v1/repos/{owner}/{repo}/stargazers?limit=30&page={page}",
            headers={
                "User-Agent": "Ruud's Star Counting Script <dev@ruuda.nl>",
            },
        )
        with client.getresponse() as response:
            assert response.status == 200, response.read()
            data = json.load(response)
            n += len(data)
            if len(data) < 30:
                break

    return n


def count_stars_github(client: HTTPSConnection, owner: str, repo: str) -> int:
    n = 0
    page = 0

    while True:
        # There is a Link header with a link to the next and last page, but it
        # is annying to parse, we'll just iterate until we no longer get 30
        # results in the page.
        page += 1
        client.request(
            "GET",
            f"/repos/{owner}/{repo}/stargazers?per_page=30&page={page}",
            headers={
                "Accept": "application/vnd.github+json",
                "User-Agent": "Ruud's Star Counting Script <dev@ruuda.nl>",
                "X-GitHub-Api-Version": "2022-11-28",
            },
        )
        with client.getresponse() as response:
            assert response.status == 200, response.read()
            data = json.load(response)
            n += len(data)
            if len(data) < 30:
                break

    return n


def main() -> None:
    now = datetime.now(tz=timezone.utc)
    result: Dict[str, int] = {}

    timeout_seconds = 9.1
    client_gh = HTTPSConnection("api.github.com", timeout=timeout_seconds)
    client_cb = HTTPSConnection("codeberg.org", timeout=timeout_seconds)

    for repo in load_repos():
        count_gh = count_stars_github(client_gh, "ruuda", repo.slug)
        count_cb = count_stars_forgejo(client_cb, "ruuda", repo.slug)
        result[repo.slug] = count_gh + count_cb

    data = {
        "generated_at": now.isoformat(),
        "stars": result,
    }

    with open("stars.json", "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2)


if __name__ == "__main__":
    main()
