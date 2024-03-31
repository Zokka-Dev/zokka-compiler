+ Standup an online service allowing people to setup personal and company custom
  repositories very easily. This would include a new reference implementation of
  a Zokka custom repository that people can standup individually should they so
  choose.
+ Fix up log messages and code hygiene. Right now there's a lot of hacky and
  borderline nonsensical print statements that I've thrown in for personal
  debugging.
+ Set up consistent benchmarks of compile and runtime performance.
+ Implement the learnings from elm optimize level 2.
+ Random other bugfixes as they come in.

## Standup custom repositories

Since Zokka's inception it has been possible in theory to publish to a custom
repository that you standup with
[https://github.com/changlinli/zokka-package-website](https://github.com/changlinli/zokka-package-website).
In practice though, this is quite annoying and difficult and without doing this,
the utility of Zokka's custom repository support is severely curtailed (although
`single-package-locations` are still very useful). It would be much better to
just have some website where you can create a custom repository with a single
click and have a reference implementation of that website that anyone could
use on their own servers.

The ability to quickly spin up and destroy custom repositories opens a wide
range of new workflow possibilities.

+ Users could treat these custom repositories as "staging grounds" for publishing
  packages. Right now every new package published to the Elm package repository
  must be treated as a production-ready, immutable package, which makes package
  publishing much more high-stakes than it needs to be. These custom
  repositories can act as an intermediary.
    * New users can use this as a chance as a low-stakes method of getting
      familiar with how to publish an Elm package
    * All users can use this as a method for disseminating informal, beta
      versions of libraries to a select group of testers
+ Organizations might want their own custom repositories. Larger orgs might be
  willing to eat the operations cost of setting up the package website and
  maintaining it, but smaller orgs might just want a turnkey solution.
+ Both orgs and individuals might simply not want to use GitHub

Some notes:

+ These custom repositories will be unchained from `git` and GitHub and will
  directly store packages on the server.
    * This may drastically increase server operation costs
    * I will likely institute very strict limits on upload/download frequency,
      package sizes, and overall repository sizes as a result.
    * There may be a paid option with higher limits to cover server fees if
      there's demand to raise those limits.
+ Because these custom repositories may be ephemeral and/or mutable at the
  discretion of their owners:
    * The custom auth tokens used in lieu of GitHub handling authentication and
      publish authorization will be *required* and no option will be given to
      allow for explicitly "globally readable" packages.
    * Moreover, I don't want to encourage fragmentation of the package ecosystem
      among many servers, so if you want a package to globally readable, I would
      still direct you first to the standard Elm package repo and failing that
      to the standard Zokka repo.
+ As a result, the standard Zokka repository at
  [https://zokka-lang.com](https://zokka-lang.com) will *not* follow this design.

## Fix up log messages and code hygiene

I've not really been paying attention to keeping code very hygienic as I'm
implementing things. That really should change to make sure that we keep the
codebase healthy.

There's also some architectural notes and docs that probably need to be written
up in service of this.

## Consistent benchmarks of compile and runtime performance

It would be useful as we continue making various tweaks to the compiler to
ensure that we aren't giving up performance either at compile or runtime. This
would require us to set up infrastructure directly aimed at benchmarking.

Note that we can't use usual CI/CD services for this because those services
usually have a multitenant architecture, or are prohibitively expensive for a
free project when guaranteeing exclusive use of the underlying hardware. If
don't have exclusive access to the underlying hardware, we can see many spurious
performance changes that have nothing to do with any changes we make and are
simply because another CI/CD user was running a build at the same time on the
same underlying hardware.

There's also some OS-level tweaks we should make to help have more repeatable
results. We should look at how the Scala compiler when it was trying to combat
compilation speed woes
([https://github.com/scala/scala-dev/issues/338](https://github.com/scala/scala-dev/issues/338)).

## Implement the learnings from elm optimize level 2

These optimization techniques could potentially be done much more quickly (i.e.
they run faster at compile time) within the compiler than as JS transformations
after a JS artifact has already been produced.

Because they are also JS-level transformations, they should be easier to
implement than wholly novel transformations which may require changes much
earlier in the compilation pipeline.

Let's look at this in conjunction with the compile time and runtime benchmarks
to get a sense of how much improvement we get at runtime and how much we're
paying for it at compiletime. Should have some set limit of how much compile
time performance we're willing to sacrifice. Even if we do stick this behind
some other flag to opt-in, we should still keep a close eye on the compiletime
performance numbers.
