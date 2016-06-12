Common prelude for all Holborn code.

Inspired by
[basic-prelude](https://hackage.haskell.org/package/basic-prelude), the idea
is to have something that is fairly small in surface, but implements Holborn
house style.

## Assumptions

* You are using `-XOverloadedStrings`

## Usage

* Add as a dependency to your library / executable
* Compile your thing with `-XNoImplicitPrelude`
* `import HolbornPrelude` at the top of your file

## Plans

* Stephen Diehl has a bunch of great ideas in his
[protolude](http://www.stephendiehl.com/posts/protolude.html) post.
* We should make sure only total functions are exported (see `Safe`)
* Export many `Control.Errors` utilities
* Dedicated functions for certain kinds of errors (`notImplementedYet`,
  `assertion`)
* Export debugging functions with warnings enabled
* Re-export `Control.Applicative` lock, stock, and barrel
* Define & re-export `unit = pure ()`
* Switch to typeclass-y versions of things like concat
* Export things we use a lot:
  * string conversion?
  * certain transformers
* Move to be organized more like protolude
* Prefer Traversable, Foldable to concrete functions
