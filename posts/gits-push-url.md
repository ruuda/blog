---
title: Git’s push url
date: 2016-05-10
minutes: 1
synopsis: Here’s a little-known feature of Git that allows you to fetch over https but push with SSH.
run-in: Fetching from
---

Fetching from a public Git repository over https is convenient
because it does not require any credentials.
But to push, I’d rather use SSH.
This means setting the remote url to the SSH one for repositories that I have push access to.
I rarely use an SSH agent that can handle ed25519 keys,
and unfortunately that means I have to unlock my key every time I pull or fetch,
even when authentication is not required.

A little-known feature of Git comes in handy here: [`remote.pushurl`][pushurl-docs].
To change the push url for a repository,
run `git config -e` to open up the repository configuration in an editor.
Every remote has a section here:

```ini
[remote "github"]
  url = git@github.com:ruuda/blog
  fetch = +refs/heads/*:refs/remotes/github/*
```

The single url there is used both for pushing and fetching.
To fetch via https but push over SSH,
change it as follows:

```ini
[remote "github"]
  pushurl = git@github.com:ruuda/blog
  url = https://github.com/ruuda/blog
  fetch = +refs/heads/*:refs/remotes/github/*
```

The fact that those urls line up so nicely makes me happier than it should.

[pushurl-docs]: https://git-scm.com/docs/git-push#_named_remote_in_configuration_file
