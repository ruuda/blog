---
title: New PGP key
date: 2025-07-31
lang: en-US
synopsis: I revoked my former key with id 5F231E540599697D. 284FE5A783926532 is my new key.
run-in: Last Tuesday I made a big mistake
---

Last Tuesday I made a big mistake.
I inadvertently shared an encrypted copy of my PGP secret key with six people,
whom I intended to send my public key.
The key with id 5F231E540599697D should be considered compromised,
and signatures encountered after 2025-07-29 should not be trusted.
Signatures _verified_ before that date are still fine.
GitHub still marks previously pushed tags that were signed with this key as verified,
though Codeberg does not now that I removed the key there.

I revoked the former key.
You can download the public key,
including revocation certificate,
<a href="/contact/ruuda-5F231E540599697D-revoked.asc" title="Revoked key 5F231E540599697D">here</a>.
I published it to keys.openpgp.org as well,
in the hope that this makes it harder
to accidentally find the un-revoked version.

My new key has id 284FE5A783926532,
and you can download the public key
<a href="/contact/ruuda-284FE5A783926532.asc" title="New key 284FE5A783926532">here</a>.
