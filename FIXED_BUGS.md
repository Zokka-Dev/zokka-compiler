# List of bugs 

These are a list of known bugs in version 0.19.1 of the Elm compiler which
Zokka fixes.

+ String literals are not sanitized when generating HTML files:
  [https://github.com/elm-lang/elm-make/issues/174](https://github.com/elm-lang/elm-make/issues/174)
  linked from
  [https://github.com/elm/compiler/issues/1377](https://github.com/elm/compiler/issues/1377)
+ Improper tail-call optimization leading to inconsistent, sometimes crashing
  code when using closures (this actually was two separate bugs which are both
  fixed):
    - [https://github.com/elm/compiler/issues/2017](https://github.com/elm/compiler/issues/2017)
    - [https://github.com/elm/compiler/issues/1813](https://github.com/elm/compiler/issues/1813)
    - [https://github.com/elm/compiler/issues/2268](https://github.com/elm/compiler/issues/2268)
    - [https://github.com/elm/compiler/issues/2221](https://github.com/elm/compiler/issues/2221)
