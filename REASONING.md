# What is Zelm?

Zelm is an alternative compiler for Elm, forked off the same underlying
codebase, alongside some additions to the Elm package ecosystem. Its main
purpose is to provide a method for bug fixes and patches that have accumulated
over the years for the compiler and foundational packages to be used by any Elm
programmer.

To accomplish this, compared to the standard Elm compiler, Zelm adds:

1. The ability to use custom package repositories
2. The ability to override dependencies (both direct and direct) with custom
   versions of your own

In addition to providing a method to incorporate bug fixes and patches, Zelm
also provides them directly. That means it includes on an ongoing basis:

1. A collection of API-identical versions of foundational Elm packages that
   incorporate bug fixes that can be used to override their usual counterparts
2. A minimal alternative package repository that hosts these packages
3. Minor compiler bug fixes and quality of life improvements *that do not change
   the Elm language* (ongoing)

This aims to be the entirety of Zelm's mission through the end of 2024. This
means Zelm is a quite conservative extension of Elm. What this implies is:

+ A commitment to bidirectional compatibility with Elm 0.19 through the end of
  2024: any code that compiles with the default Elm compiler should compile with
  Zelm *and vice versa*. In particular, this means that Zelm will not just be
  backwards-compatible with Elm, all Zelm code that compiles with the Zelm
  compiler should compile with the Elm compiler, barring compiler bugs in the
  Elm compiler (or Zelm compiler!) or usage of packages not in the standard Elm
  package repository.
+ What is considered a "bug fix" will be quite tightly scoped. E.g. something
  like [https://github.com/elm/compiler/issues/1773](https://github.com/elm/compiler/issues/1773])
  that adds the ability to pattern match on negative number literals would be on
  the fence (and likely not considered to be part of Zelm's current mission).
  Common candidates for compiler bug fixes that would be considered acceptable
  are compiler crashes and improved error messages.
+ **Very little actual feature development.** Some experimentation may happen on side
  branches that may show up in both private and public channels, but those will
  not be given an actual Github release until 2025 (if Zelm is still relevant
  then).

**Again, Zelm's main function in the wider Elm community (at least until 2025)
is to collect bug fixes submitted by the community and merge them, not to do
major Elm development itself.** As a (substantial) side effect of that Zelm
provides private repositories as well, largely in part because this allows
individuals to fix bugs privately and use those bug fixes without exposing them
to the wider world if they would like.

# How do I use Zelm?

## Quick Start

1. Download the Zelm binary from Github releases. 
2. Run `zelm make $YOUR_ELM_MAIN_FILE` with whatever flags you usually pass to
   the Elm compiler.
3. Everything should work just the exact same as it did before!

### Explanation of quick start

`zelm` aims to be command-line compatible with the usual `elm` binary (except
for the `publish` command), so all `elm` subcommands are usable in `zelm` as
well with their usual flags. So for example you can immediately run `zelm make
$YOUR_ELM_MAIN_FILE` in the root of a pre-existing Elm project and it should run
fine. Indeed if you wanted you could alias `zelm` as `elm`.


## Using Custom Repositories

To take advantage of Zelm-specific support for custom repositories, you can
look at the `custom-package-repository-config.json` file, which is located in
the `$ELM_HOME/zelm/` directory (this is usually `~/.elm/zelm/`). Much like how
`$ELM_HOME` is generated on the first invocation of an `elm` subcommand, this
file is generated the first time you successfully run a `zelm` subcommand (e.g.
`zelm make`). By default the file looks like the below:

```
{
    "repositories": [
        {
            "type": "elm-package-server",
            "url": "https://package.elm-lang.org/"
        },
        {
            "type": "elm-package-server",
            "url": "https://package.zelm-lang.org/"
        },
    ],
    "single-package-locations": []
}
```

The `custom-package-repository-config.json` file controls all configuration of
package repositories used by `zelm`.

As can be seen, by default this configuration file includes both the standard Elm
package repository as well as a separate Zelm package repository. Either
repository can be deleted. For example if you would like to make use of the Zelm
compiler with its bug fixes, but would otherwise like to ensure that you have a
vanilla Elm project,  you can delete the `zelm-lang.org` repository.

If you wish to set up a private repository for e.g. CI/CD purposes, you can
delete both default repositories and include only the URL to your private
repository.

The set of packages available to the `zelm` compiler to download and compile is
the union of all packages among your repositories.

## Dependency Overrides

Sometimes there are bug fixes to packages to which you cannot contribute the bug
fix. For most direct dependencies this is not a big deal: you can fork the
package and depend on your fork. However, for packages that are indirect
dependencies or packages that are in the `elm` or `elm-explorations` GitHub
organizations that is not feasible.

Zelm lets you instead override dependencies. So for example if you depend on
`elm/core` version `1.0.5`, but would like to incorporate the bug fix in
https://github.com/elm/core/pull/1092, you can add `zelm-package-overrides` to
your `elm.json` and run `zelm make`.

```
{
    "type": "application",
    "source-directories": ...,
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.0",
            ...
        },
        "indirect": { ... }
    },
    "test-dependencies": {... },
    "zelm-package-overrides": [
        {
            "original-package-name": "elm/core",
            "original-package-version": "1.0.5",
            "override-package-name": "zelm/core-1-0-override",
            "override-package-version": "1.0.1"
        }
    ]
}
```

The way to read this declaration is that we are overriding version `1.0.5` of
the package named `elm/core` using version `1.0.1` of the package named
`zelm/core-1-0-override`.

Note that `zelm/core-1-0-override` exists only on
`https://package.zelm-lang.org/` not the standard Elm package repository
`https://package.elm-lang.org/`, so you'll need to make sure that the former is
present in `$ELM_HOME/zelm/custom-repository-config.json`.

Packages used to override other packages should:

1. Be API-compatible with the original package. I.e. use the exact same module
   names and function/value names.
2. Be clearly marked as a package to be used to override other packages. E.g. by
   convention all the Zelm packages used as overrides are named
   `$ORIGINAL_PACKAGE_NAME-$ORIGINAL_PACKAGE_MAJOR_MINOR-override`.
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
`zelm-package-overrides`. By keeping package overrides off of the standard Elm
repository and clearly labeling such packages as overrides, we prevent
unsuspecting Elm developers who aren't using Zelm from inadvertently ending up
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

## Publishing Packages

The `zelm publish` command is a bit different from `elm publish`. It takes a
mandatory argument, which is the URL of the package repository (running an
instance of the Elm package repository server) you wish to publish to.

**Zelm is hard-coded to disallow publishing to the
https://package.elm-lang.org/ package repository to preserve the integrity of
the standard Elm repository**. While Zelm packages could depend on packages that
do not exist in the standard Elm repository, this is not the most important
reason we disallow publishing to the standard Elm repository, since there are
ways we could detect this and just selectively disallow those cases. The biggest
reason is that the Zelm compiler may include bug fixes for Elm compiler crashes,
which means that if we want to make sure that a Zelm-published package on the
standard Elm package repository is usable in a vanilla Elm project, we must
replicate all of the Elm compiler's bugs, which goes against one of the goals of
Zelm. Otherwise we could end up with a Zelm-published package that causes a
compiler crash when used in a vanilla Elm project.

**If you wish to publish a package developed with Zelm to the standard Elm
package repository, take advantage of bidirectional compatibility and use the
standard Elm binary instead to publish the package**. Assuming you are not using
packages from non-stanard repositories, this should be a drop-in substitution
(file a bug in Zelm if this is not the case!). You can still use Zelm while
developing your package so that you can enjoy Zelm's improved error messages and
other small quality of life improvements, but you are also thereby guaranteed
that whatever you publish on https://package.elm-lang.org/ is usable by other
Elm developers.

If you would like you can also manually publish a package. The easiest way to do
this is just to bundle it up as a zipfile, throw it up somewhere, and then
expose that location as a new entry under `single-package-locations` in your
`custom-package-repository-config.json` file in `$ELM_HOME/zelm`.

# Social Dynamics

In light of Zelm's tightly scoped technical goals as well as the current state
of Elm's community.

In particular Elm's community is:

+ Stable: the number of Elm veterans in the community outnumbers the number of
  new Elm developers.
+ Relatively consistent when it comes to opinions about what Elm code should
  look like: 

In terms of Zelm's own social goals, there is really just one big one.

+ 

And as a reminder Zelm does not intend (at least not until 2025) to do feature
development on the Elm language itself.

This mean that Zelm does not really need a visionary at its helm or large
amounts of time and energy spent on "wow"-esque features. What Zelm needs
instead foremost is a low, but consistent, amount of attention to accept PRs and
bug reports from the community. Secondarily, as time allows, Zelm will slowly
grind through bugs that have no PRs.

The absence of a need for radical design or commitment and instead a focus on a
consistent source of low amounts of energy makes Zelm a good fit for committee
leadership rather than individual leadership.

As such Zelm hopes to

+ Maintain at least five active members of the `zelm` and `zelm-explorations`
  GitHub organizations who have push/merge permissions across the repositories
  managed by those organizations. Potentially more than five, but ideally
  remaining at some odd number to allow for tie breaks in the (hopefully rare)
  case of controversial decisions.
+ Regularly rotate out any inactive members (people who do not feel they can
  engage more than once a week) for active members.

**These are hopes not commitments.** But I do consider them valid barometers of
whether Zelm has a project has succeeded.


Because Zelm does not intend (at least not until 2025) to do feature
development on the Elm language itself, Zelm makes a different set of social
trade-offs compared to Elm itself.

Concretely, Zelm hopes to achieve timely (< 1 week) responses to issues and PRs
that fit within Zelm's mission scope, i.e. bug fixes to foundational packages or
to the compiler. These should be fixes with very few or no questions of design
and no changes to any APIs.

**Much like the technical goals, the social goals of Zelm are very tightly
scoped to this goal of responsiveness**.

I hope that by intentionally side-stepping questions of 

Zelm prefers to lead by committee rather than individual.

We hope to have a stable roster of at least five people managing `zelm` and
`zelm-explorations`, maybe more (although for tie-breaking purposes I'd like to
keep the number odd).

# FAQ

## The design of Zelm

+ Why have a separate `custom-package-repository-config.json` file in
  `$ELM_HOME` instead of just extending `elm.json` even more? Zelm aims to be
  compatible with current Elm tooling, especially IDE tooling. Current IDE
  integrations assume that there is a global cache of packages located in
  `$ELM_HOME` rather than local per-project caches (as is e.g. the case with
  `npm`). This means that to play nicely with these tooling choices, Zelm also
  reuses the same global cache. However, because the cache is global to a
  machine, while a machine can have many Elm projects, if one 

