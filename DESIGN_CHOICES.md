## Compatibility Constraints

Zokka aims to be:

1. As in `README.md`, bicompatible with the Elm compiler when it comes to a
   given piece of Elm 0.19.1 code.
2. Not interfere with the execution of the vanilla Elm compiler, i.e. a user
   should be able to switch back and forth arbitrarily between invocations of
   `zokka` and `elm` without the need to perform any sort of clean-up operation
   in-between, such as deleting `elm-stuff` or `$ELM_HOME`.
3. Be reasonably compatible with existing IDE tooling. We add the caveat
   "reasonably," because without any updates to the IDE tools, we cannot ensure
   that those tools take dependency overrides into account when providing
   features such as "click to definition." However, outside of zokka-specific
   features, we should preserve IDE compatibility as much as possible.

This has a variety of downstream effects on technical choices that Zokka has
made, as elaborated further in the FAQs.

