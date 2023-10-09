## Don't use custom zelm.json append to elm.json

Keep this compatible with existing Elm tooling.

## Put custom repository info in ~/.elm not in elm.json/zelm.json

Custom repositories affect the global Elm cache and so should live globally lest
different projects end up with conflicting ideas of what should be put in the
cache.

## What to do about SHA hashes?

Would like to have hashes for packages, but so far not sure exactly when the
check would be (I guess on install?).

## Only allow overrides of single package versions not ranges

Makes implementation *way* easier (otherwise have to come up with some sort of
custom mapping syntax).

Besides, overrides really should only exist in applications not libraries and
applications usually have single versions of libraries instead of ranges.

## Don't duplicate package overrides in dependencies

On the one hand this would be nice if we required that packages introduced as an
override also showed up in dependency lists to preserve the expectation that
dependency lists do in fact list *all* the packages that an application depends
on.

However, this has the potential for severe breakage because different packages
cannot have the same module names (see
https://github.com/elm/compiler/issues/1625). Not only would it be a ton of work
to modify the Elm compiler to hack around this (certainly can't fix it for good
because it breaks bidirectional compatibility), it could leave users confused
when they remove the override line and forget to remove the dependency and then
suddenly their application fails to compile (when an important and heavily
implied invariant of overrides is that a project would at least build even
without the override).
