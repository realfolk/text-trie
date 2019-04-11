text-trie
===============
[![Hackage version](https://img.shields.io/hackage/v/bytestring-trie.svg?style=flat)](https://hackage.haskell.org/package/text-trie)
[![Build Status](https://travis-ci.org/michaeljklein/text-trie.svg?branch=master)](https://travis-ci.org/michaeljklein/text-trie)

The `text-trie` package is a lightweight adaptation of `bytestring-trie` to `Text`.

For the differences in performance, see [bench.md](https://github.com/michaeljklein/text-trie/blob/text-trie/bench.md).


## bytestring-trie

The [bytestring-trie](https://github.com/wrengr/bytestring-trie) package provides an efficient implementation
of tries mapping `ByteString` to values.  The implementation is
based on Okasaki's big-endian patricia trees, à la `IntMap`.  We
first trie on the elements of `ByteString` and then trie on the
big-endian bit representation of those elements.  Patricia trees
have efficient algorithms for union and other merging operations,
but they're also quick for lookups and insertions.

If you are only interested in being able to associate individual
`ByteString`s to values, then you may prefer the `hashmap` package
which is faster for those only needing a map-like structure.  This
package is intended for those who need the extra capabilities that
a trie-like structure can offer (e.g., structure sharing to reduce
memory costs for highly redundant keys, taking the submap of all
keys with a given prefix, contextual mapping, extracting the minimum
and maximum keys, etc.)


## Install

This is a simple package and should be easy to install.  You should
be able to use one of the following standard methods to install it.

```bash
    -- With stack and without the source:
    $> stack install text-trie
    
    -- With stack and with the source already:
    $> cd text-trie
    $> stack install
    
```


## Portability

The implementation only relies on a few basic
language extensions and `DeriveGeneric`. The complete list of extensions used is:

* `CPP`
* `MagicHash`
* `NoImplicitPrelude`
* `StandaloneDeriving`
* `DeriveGeneric`


## Links

- [Hackage](http://hackage.haskell.org/package/text-trie)
- [GitHub](https://github.com/michaeljklein/text-trie)

- `bytestring-trie`
  * [Website](http://wrengr.org/)
  * [Blog](http://winterkoninkje.dreamwidth.org/)
  * [Twitter](https://twitter.com/wrengr)
  * [Hackage](http://hackage.haskell.org/package/bytestring-trie)
  * [GitHub](https://github.com/wrengr/bytestring-trie)

