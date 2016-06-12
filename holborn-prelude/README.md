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
* We could/should make sure only total functions are exported (see `Safe`)
* Export `Control.Errors` utilities
