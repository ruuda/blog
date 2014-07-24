---
title: Passphrase entropy
date: 2012-07-07 17:35
---

While I was researching passphrase entropy today, I made a shocking discovery.
_Virtually any passphrase that a human can remember,
does not have enough entropy to generate a strong cryptographic key._
Even though most modern encryption algorithms use key sizes of 128 or 256 bits,
a comparatively 'strong' passphrase has nowhere near this amount
of entropy bits.

<!--more-->

Entropy
-------
Entropy in information theory, is related to the number of possible values
of a variable. The more possibilities, the more entropy.
Entropy makes it difficult to do a brute-force attack, and it plays a role
in several other types of attack. For a brute-force attack, the relation
is easy to see: if a variable can have many different values, it will
require much work to try all possible values. If the variable can take on
only a limited amount of values, trying them all might not require
that much work. Entropy is a desirable property of cryptographic keys.

The power of encryption lies in the large keyspace (the set of possible
keys). Probably the most common algorithm nowadays is Rijndael with a 256-bit key.
Brute-forcing a 256-bit key is simply
[infeasable](http://crypto.stackexchange.com/questions/1145/how-much-would-it-cost-in-u-s-dollars-to-brute-force-a-256-bit-key-in-a-year),
so a 256-bit key can be considered secure. However, this is
only true if the key is random -- that is, if the key
has 256 bits of entropy.

Deriving keys
-------------
In many cases, the key cannot be stored. It must be remembered by a human,
so data cannot be decrypted without this person's knowledge. Unfortunately,
humans are not every good at remembering a sequence of 256 bits.
Even when expressed octally, it is still a sequence of 64 random digits.
This is where key derivation comes into play. A key derivation algorithm
generates a cryptographic key, given a passphrase. This way, a human
can simply remember the passphrase,
and the key can be generated wherever it is required.
Even though several key derivation algorithms can convert passphrases of
arbitrary length to arbitrary length keys, this does not mean that
_entropy_ increases. If a passphrase would be limited to five different
words, there would be only five diffent keys. A key derivation algorithm
cannot directly increase the entropy of a passphrase.

Typical passphrase entropy
--------------------------
As suggested by [xkcd](https://xkcd.com/936/), one can use a combination
of four common words to make up a 'strong' passphrase. The comic assumes
entropy of eleven bits per word, resulting in 44 bits of entropy
for the entire passphrase. To see how this number is derived,
we must adjust our view on the brute-force attack. As it is infeasable
to brute-force all 2<sup>256</sup> keys, a different strategy is required.
If we assume that all passphrases are combinations of four words,
then we can simply try all possible combinations of four words as a
brute-force attack. To do so, one needs a dictionary of words.
The _Van Dale Pocket Dictionary English - Dutch_ contains roughly 23000 words,
which should include the most common English words. If we assume that only
words from this dictionary can be used for passphrases, and we start with
one-word passphrases, then there are 23000 unique options to try.
The number of bits required to identify a value among 23000 possibilities,
is log<sub>2</sub>(23000) ≈ 14.5 bits. (The comic assumes eleven bits per word,
so it must have used a smaller dictionary.) For four words,
the entropy is four times as large: 58 bits (or 44 in the comic).

It is possible to try some variations on the four-word scheme,
but this does not add a significant abount of entropy. For example,
making the first character of every word either uppercase or lowercase
(instead of solely lowercase), adds only one bit of entropy per word.
Appending a random number between zero and a thousand,
adds only ten bits of entropy.

Key stretching
--------------
As it turns out, some key derivation algorithms _can_ be used to increase
the entropy of a key. Most key derivation algorithms run a large number
of iterations, which makes the derivation computationally expensive.
Brute-forcing a certain amount of keys, is computationally expensive as well.
[Key stretching](https://www.schneier.com/paper-low-entropy.html)
relies on the principle that deriving the key in an expensive way,
requires an amount of work equal to brute-forcing a certain number of keys.
If a key of ten bits can be brute-forced in five seconds, then requiring
the key derivation to take five seconds effectively increases the entropy of the key
with ten bits. This technique can be used to increase the entropy
of a key with at most a few dozen bits, but still
-- that is nowhere near the 256 available bits.

Solutions
---------
I have not found a good solution to this problem yet.
(If you have, feel free to share it with me.)
The most obvious solution is to have an alternative source of entropy; a keyfile.
However, the keyfile has several other problems that make its source of entropy rather useless.
The keyfile must be stored, which is a major problem in itself.
After all, keyfiles only make it harder for an attacker to find the key.
In the end -- even if you have a perfectly random key --
there are always [alternative ways](https://xkcd.com/538/) for an attacker
to break your encryption.
