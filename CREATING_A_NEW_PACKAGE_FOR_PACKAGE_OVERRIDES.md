Packages used to override other packages should:

1. Be API-compatible with the original package. I.e. use the exact same module
   names and function/value names.
2. Be clearly marked as a package to be used to override other packages. E.g. by
   convention all the Zokka packages used as overrides are named
   `$ORIGINAL_PACKAGE_AUTHOR-$ORIGINAL_PACKAGE_NAME-$ORIGINAL_PACKAGE_MAJOR_MINOR-override`.
3. Not be published on the standard Elm package repository.
4. Not be used as a normal dependency by any other package or application.
5. Shoud have the same dependencies as the original package.

The first rule perhaps has the clearest motivation.  Packages used to override
other packages must be API-compatible with the original package, otherwise you
can end up with unfixable compilation errors in your dependencies if you are
overriding an indirect dependency. Indeed it would be advisable to have the APIs
be identical and any API extensions in the overriding package be part of a
separate package. This rule is not rigorously machine-checked except as a
byproduct of compilation when used in a project. Hence if you are publishing a
package meant to override another package, you must keep in mind not to change
the API of the original package.

The next three rules are all mainly consequences of the fact that the Elm
compiler cannot handle different packages that have the same module names. That
will result in a compile error where the compiler refuses to compile either
package if they are used in the same project. Therefore we want to minimize the
chances where someone could end up either directly or indirectly depending on
both a package and another API-identical package meant to override the former.
Packages meant to override other packages should only ever show up in
`zokka-package-overrides`. By keeping package overrides off of the standard Elm
repository and clearly labeling such packages as overrides, we prevent
unsuspecting Elm developers who aren't using Zokka from inadvertently ending up
with a project that has uncompilable, conflicting dependencies.

The fifth rule has to do with the nature of dependency resolution. If your
overriding package could have different dependencies than the package being
overridden you could end up with an enirely different set of indirect
dependencies after using a dependency override. This greatly complicates the
dependency resolution process and raises some thorny design questions of whether
the `elm.json` file should change to accomodate these new dependencies and how
this ties in with our desire to keep our `elm.json` file as bidirectionally
compatible with vanlla Elm as possible. As a result we are side-stepping these
issues by mandating that your overriding package must have the same
dependencies.
