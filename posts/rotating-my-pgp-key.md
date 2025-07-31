---
title: Rotating my PGP key
date: 2025-08-01
lang: en-US
synopsis: I revoked my former PGP/GPG key on July 29, 2025.
run-in: Last Tuesday
---

Last Tuesday I inadvertently shared an encrypted copy of my PGP secret key with six people,
to whom I intended to send my public key.
The key with id `5F231E540599697D` should be considered compromised,
and signatures encountered after July 29, 2025, should not be trusted.
Signatures _verified_ before that date remain valid.

I revoked the former key.
You can still download the public key,
including revocation certificate,
<a href="/contact/ruuda-5F231E540599697D-revoked.asc" title="Revoked key 5F231E540599697D">here</a>.
I published it to keys.openpgp.org and keyserver.ubuntu.com as well,
in the hope that this makes it harder
to accidentally find a non-revoked version.

My new key has id `284FE5A783926532`,
and you can download the public key
<a href="/contact/ruuda-284FE5A783926532.asc" title="New key 284FE5A783926532">here</a>.

## Impact

Aside from securing my personal email,
I had used this key to sign tags in personal Git repositories.
GitHub still marks previously pushed tags that were signed with the key as verified,
though Codeberg does not,
now that I removed the key there.

I estimate that the risk of unauthorized signing is very low.
The leaked key was encrypted with a strong passphrase,
and sent over an encrypted channel to individuals
who I know and trust to not abuse the key directly,
even if the passphrase were cracked.
Still, this is a compromise,
so I am revoking and rotating the key.

## What happened

The short version is,
I made a typo in the following shell command:

    gpg --export --armor $email ~/ruuda.asc

There should have been a `>` before the final argument.
I intended to overwrite `~/ruuda.asc` with the latest version of my public key.
What this `gpg` command did instead,
is print it to stdout, and then silently ignore the second argument.
The file that I intended to overwrite,
contained a passphrase-protected export of my secret key.
Thinking that I had overwritten it with the public key,
I then attached the file to a PGP-encrypted email.

## Lessons

The main lesson for me is:
don’t use the same extension for both secret and public keys.
Mark secret keys clearly in the file name.
Also, I should double-check the contents of attachments before sending,
even when I think I saved them correctly.
