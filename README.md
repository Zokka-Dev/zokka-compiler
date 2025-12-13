**The Zokka compiler is a fork of the Elm compiler. The original README that
comes with the Elm compiler can be found at the end of this document.**

# What is Zokka?

Zokka is an alternative compiler for Elm, forked off the same underlying
codebase, alongside some additions to the Elm package ecosystem. Its main
purpose is to provide a method for bug fixes and patches that have accumulated
over the years for the compiler and foundational packages to be used by any Elm
programmer.

To accomplish this, compared to the standard Elm compiler, Zokka adds:

1. The ability to use custom package repositories
2. The ability to override dependencies (both direct and indirect) with custom
   versions of your own

In addition to providing a method to incorporate bug fixes and patches, Zokka
also provides them directly. That means it includes on an ongoing basis:

1. A collection of API-identical versions of foundational Elm packages that
   incorporate bug fixes that can be used to override their usual counterparts [this is not quite done yet]
2. A minimal alternative package repository that hosts these packages
3. Minor compiler bug fixes and quality of life improvements *that do not change
   the Elm language* (ongoing)

This aims to be the entirety of Zokka's mission until 2026. This
means Zokka is a quite conservative extension of Elm. What this implies is:

+ A commitment to bidirectional compatibility with Elm 0.19 through the end of
  2026: any code that compiles with the default Elm compiler should compile with
  Zokka *and vice versa*. In particular, this means that Zokka will not just be
  backwards-compatible with Elm, all Zokka code that compiles with the Zokka
  compiler should compile with the Elm compiler, barring compiler bugs in the
  Elm compiler (or Zokka compiler!) or usage of packages not in the standard Elm
  package repository.
+ What is considered a "bug fix" will be quite tightly scoped. E.g. something
  like [https://github.com/elm/compiler/issues/1773](https://github.com/elm/compiler/issues/1773)
  that adds the ability to pattern match on negative number literals would be on
  the fence (and likely not considered to be part of Zokka's current mission).
  Common candidates for compiler bug fixes that would be considered acceptable
  are compiler crashes and improved error messages.
+ **Very little actual feature development.** Some experimentation may happen on side
  branches that may show up in both private and public channels, but those will
  not be given an actual Github release until 2026 (if Zokka is still relevant
  then).

**Again, Zokka's main function in the wider Elm community (at least until 2026)
is to collect bug fixes submitted by the community and merge them, not to do
major Elm development itself.** As a (substantial) side effect of that Zokka
provides private repositories as well, largely in part because this allows
individuals to fix bugs privately and use those bug fixes without exposing them
to the wider world if they would like.

# How do I use Zokka?

## Quick Start

You can always drop in Zokka as a replacement for Elm so for example the
following will work.

1. Download the Zokka binary from Github releases
   [https://github.com/changlinli/zokka-compiler/releases](https://github.com/changlinli/zokka-compiler/releases). 
2. Run `zokka make $YOUR_ELM_MAIN_FILE` with whatever flags you usually pass to
   the Elm compiler.
3. Everything should work just the exact same as it did before!

But that's not terribly interesting so let's look at
[https://github.com/changlinli/zokka-basic-example](https://github.com/changlinli/zokka-basic-example).
This repository showcases a bug in the `elm/core` library. Normally we would be
stuck with this bug and have to work around it, because we cannot fork
`elm/core` and use with the vanilla Elm compiler.

Zokka, however, allows this with a single override in your `elm.json`. Let's
demonstrate that.

1. Run `git clone https://github.com/changlinli/zokka-basic-example` to get the
   repository.
2. `cd zokka-basic-example`.
3. Now let's first run vanilla Elm: `elm make src/Main.elm`.
4. Open the resulting `index.html` in your web browser of choice. Get ready to
   close the browser, because it should hang in an infinite loop!
5. Now let's try again with Zokka: `zokka make src/Main.elm`.
6. Again open the resulting `index.html` in your web browser of choice. You
   should now see a single string and not have a hanging browser!

### Explanation of quick start

`zokka` aims to be command-line compatible with the usual `elm` binary (except
for the `publish` command), so all `elm` subcommands are usable in `zokka` as
well with their usual flags. So for example you can immediately run `zokka make
$YOUR_ELM_MAIN_FILE` in the root of a pre-existing Elm project and it should run
fine. Indeed if you wanted you could alias `zokka` as `elm`.

Now as for the `zokka-basic-example` repository, let's go ahead and take a look
at the `elm.json`. You'll see that in addition to all the normal bits in a
vanilla `elm.json`, we also have a new field called `zokka-package-overrides`.
This field overrides `elm/core` with another package called
`zokka/elm-core-1-0-override`. `zokka/elm-core-1-0-override` fixes the bug in
`elm/core`, so by overriding `elm/core` with this package instead, we get the
bug fix!

## Using Custom Repositories

To take advantage of Zokka-specific support for custom repositories, you can
look at the `custom-package-repository-config.json` file, which is located in
the `$ELM_HOME/0.19.1/zokka/` directory (this is usually `~/.elm/0.19.1/zokka/`). Much like how
`$ELM_HOME` is generated on the first invocation of an `elm` subcommand, this
file is generated the first time you successfully run a `zokka` subcommand (e.g.
`zokka make`). By default the file looks like the below:

```
{
    "repositories": [
        {
            "repository-type": "package-server-with-standard-elm-v0.19-package-server-api",
            "repository-url": "https://package.elm-lang.org"
        },
        {
            "repository-type": "package-server-with-standard-elm-v0.19-package-server-api",
            "repository-url": "https://package-server.zokka-lang.com"
        }
    ],
    "single-package-locations": []
}
```

The `custom-package-repository-config.json` file controls all configuration of
package repositories used by `zokka`.

### Custom Multi-File Repositories

As can be seen, by default this configuration file includes both the standard Elm
package repository as well as a separate Zokka package repository. Either
repository can be deleted. For example if you would like to make use of the Zokka
compiler with its bug fixes, but would otherwise like to ensure that you have a
vanilla Elm project,  you can delete the `zokka-lang.com` repository. Likewise,
if you set up an internal package repository for your company and would like to
guarantee that all packages come from that repository and not the standard Elm
repository, you can also delete the `elm-lang.org` repository and substitute
your own.

The `package-server-with-standard-elm-package-server-api` means that those URLs
adhere to the API provided by the usual Elm package server. You can add any new
URL that also adheres to the API of the usual Elm package server as a new entry
in `repositories`. We are exploring adding other `repository-type`s.

The set of packages available to the `zokka` compiler to download and compile is
the union of all packages among your repositories.

So e.g. if you wish to set up a private repository for e.g. CI/CD purposes, you
can delete both default repositories and include only the URL to your private
repository.

`$ELM_HOME` is usually viewed as disposable, while often
`custom-package-repository-config.json` has some data that we'd rather like to
keep. One way to get around this is to store
`custom-package-repository-config.json` somewhere else and symlink it into the
proper location in `$ELM_HOME`. That way if you ever delete `$ELM_HOME`, you can
always just re-symlink your configuration.

As for why we choose to put the `custom-package-repository-config.json` file in
`$ELM_HOME`, check out the FAQs.

### Custom Single-File Repositories

Sometimes though, you don't want to spin up an entire website just to be able to
serve one or two custom packages. You might have just one package that you don't
want to publish to the public Elm repository, but still want to use in your own
projects.

In those cases, you can directly just tell Zokka the exact location of one or two
packages by adding them as entries to `single-package-locations`.

```
{
    "repositories": [
        {
            "repository-type": "package-server-with-standard-elm-package-server-api",
            "repository-url": "https://package.elm-lang.org"
        },
        {
            "repository-type": "package-server-with-standard-elm-package-server-api",
            "repository-url": "https://package-server.zokka-lang.com"
        }
    ],
    "single-package-locations": [
        {
            "file-type": "zipfile",
            "package-name": "someauthor/somecoolpackage",
            "version": "1.0.0",
            "url": "https://example.com/my-custom-library-1-0-0.zip"
            "hash-type": "sha-1",
            "hash": "somerealhashherelike293b123abb920323d127b"
        }
    ]
}
```

Note that your `package-name` and `version` must match exactly what is contained
in any `elm.json` that wishes to use that package in order for Zokka to resolve
that package correctly.

Currently `zipfile` is the only `file-type` supported, but we may add more later
on.

## Dependency Overrides

Sometimes there are bug fixes to packages to which you cannot contribute the bug
fix. For most direct dependencies this is not a big deal: you can fork the
package and depend on your fork. However, for packages that are indirect
dependencies or packages that are in the `elm` or `elm-explorations` GitHub
organizations that is not feasible.

Zokka lets you instead override dependencies. So for example if you depend on
`elm/core` version `1.0.5`, but would like to incorporate the bug fix in
https://github.com/elm/core/pull/1092, you can add `zokka-package-overrides` to
your `elm.json` and run `zokka make`.

```
{
    "type": "application",
    "source-directories": ...,
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            ...
        },
        "indirect": { ... }
    },
    "test-dependencies": {... },
    "zokka-package-overrides": [
        {
            "original-package-name": "elm/core",
            "original-package-version": "1.0.5",
            "override-package-name": "zokka/elm-core-1-0-override",
            "override-package-version": "1.0.0"
        }
    ]
}
```

The way to read this declaration is that we are overriding version `1.0.5` of
the package named `elm/core` using version `1.0.0` of the package named
`zokka/elm-core-1-0-override`.

Note that `zokka/elm-core-1-0-override` exists only on
`https://package-server.zokka-lang.com/` not the standard Elm package repository
`https://package.elm-lang.org/`, so you'll need to make sure that the former is
present in `$ELM_HOME/0.19.1/zokka/custom-repository-config.json`.

If you wish to create your own package for a package override, please see
[./CREATING_A_NEW_PACKAGE_FOR_PACKAGE_OVERRIDES.md](./CREATING_A_NEW_PACKAGE_FOR_PACKAGE_OVERRIDES.md).

### Combining Package Overrides and Custom Package Repositories

The Elm community has had some other efforts to include bug fixes to Elm's core
libraries. For example, there is the [Elm Janitor](https://github.com/elm-janitor) project, which form the
foundation of Zokka's forked versions of Elm's core libraries (thank you!).

By combining package overrides and custom package repositories, Zokka lets you
seamlessly use those custom packages if you would like. For example, if you
wish to depend solely on Elm Janitor's version of `elm/core` and not Zokka's
forked version, then you can use the following
`custom-package-repository-config.json` file, which eliminates the Zokka
repository and adds the Elm Janitor version of `elm/core` as a custom package
location.

```
{
    "repositories": [
        {
            "repository-type": "package-server-with-standard-elm-package-server-api",
            "repository-url": "https://package.elm-lang.org"
        }
    ],
    "single-package-locations": [
        {
            "file-type": "zipfile",
            "package-name": "elm-janitor/core",
            "version": "1.0.0",
            "url": "https://github.com/elm-janitor/core/archive/refs/heads/stack-1.0.5.zip",
            "hash-type": "sha-1",
            "hash": "f8b911c7d976533bce684b3c8da8f93151092bb2a"
        }
    ]
}
```

Then in your `elm.json`, you can add the following override:

```
{
    "type": "application",
    "source-directories": ...,
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/core": "1.0.5",
            ...
        },
        "indirect": { ... }
    },
    "test-dependencies": {... },
    "zokka-package-overrides": [
        {
            "original-package-name": "elm/core",
            "original-package-version": "1.0.5",
            "override-package-name": "elm-janitor/core",
            "override-package-version": "1.0.0"
        }
    ]
}
```

Note that in this case, as long as your `elm.json` and
`custom-package-repository-config.json` agree on the overriding package name and
version, you can call the package name and version whatever you want (so e.g.
instead of `elm-janitor/core`, you could've called it `some-author/some-package`
with version `5.3.1`).

However, **you should always give the overriding package a name that is
different from the original package, either by giving it a different author or a
different project name.** If you don't, this can cause problems for both Elm and
Zokka because of how Elm uses a global package cache. If for example you decide
to call your overriding package `elm/core` with version `1.0.10`, if `elm/core`
ever publishes version `1.0.10` you can end up with weird transitive dependency
errors when those two conflict.

This isn't a huge deal; you can always rename your package again if that
happens. It can just be difficult to diagnose so can cause pain and annoyance
that would be easily avoided by just making sure you give your overriding
packages names that are unique.

## Publishing Packages

**The Zokka package repository currently disables publishing to it. If you set up
your own instance of the Elm package server, you should be able to publish to it
just fine with Zokka**.

The `zokka publish` command is a bit different from `elm publish`. It takes a
mandatory argument, which is the URL of the package repository (running an
instance of the Elm package repository server) you wish to publish to.

**Zokka is hard-coded to disallow publishing to the
https://package.elm-lang.org/ package repository to preserve the integrity of
the standard Elm repository**. While Zokka packages could depend on packages that
do not exist in the standard Elm repository, this is not the most important
reason we disallow publishing to the standard Elm repository, since there are
ways we could detect this and just selectively disallow those cases. The biggest
reason is that the Zokka compiler may include bug fixes for Elm compiler crashes,
which means that if we want to make sure that a Zokka-published package on the
standard Elm package repository is usable in a vanilla Elm project, we must
replicate all of the Elm compiler's bugs, which goes against one of the goals of
Zokka. Otherwise we could end up with a Zokka-published package that causes a
compiler crash when used in a vanilla Elm project.

**If you wish to publish a package developed with Zokka to the standard Elm
package repository, take advantage of bidirectional compatibility and use the
standard Elm binary instead to publish the package**. Assuming you are not using
packages from non-stanard repositories, this should be a drop-in substitution
(file a bug in Zokka if this is not the case!). You can still use Zokka while
developing your package so that you can enjoy Zokka's improved error messages and
other small quality of life improvements, but you are also thereby guaranteed
that whatever you publish on https://package.elm-lang.org/ is usable by other
Elm developers.

If you would like you can also manually publish a package. The easiest way to do
this is just to bundle it up as a zipfile, throw it up somewhere, and then
expose that location as a new entry under `single-package-locations` in your
`custom-package-repository-config.json` file in `$ELM_HOME/0.19.1/zokka`.

# Governance Structure

See [./GOVERNANCE.md](./GOVERNANCE.md)

# Design Choices Reasoning

See [./DESIGN_CHOICES.md](./DESIGN_CHOICES.md).

# FAQs

See [./FAQs.md](./FAQs.md).

**The following is the README for the original Elm compiler:**

# Elm

A delightful language for reliable webapps.

Check out the [Home Page](http://elm-lang.org/), [Try Online](http://elm-lang.org/try), or [The Official Guide](http://guide.elm-lang.org/)


<br>

## Install

✨ [Install](https://guide.elm-lang.org/install/elm.html) ✨

For multiple versions, previous versions, and uninstallation, see the instructions [here](https://github.com/elm/compiler/blob/master/installers/README.md).

<br>

## Help

If you are stuck, ask around on [the Elm slack channel][slack]. Folks are friendly and happy to help with questions!

[slack]: http://elmlang.herokuapp.com/
