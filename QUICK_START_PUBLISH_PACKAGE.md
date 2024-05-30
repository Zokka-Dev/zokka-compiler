## A new release of Zokka with private package repositories

We have our first non-alpha (but still beta) release of Zokka! From here on out
Zokka's various configuration file formats and command line interface should not
change up until its first production release.

The big feature of this release is a new kind of package repository. This
release combines both changes to the Zokka compiler as well as the introduction
of a new package repository server that anyone can deploy on their own machines,
or can use my publically deployed version.

If you've ever wanted to be able to just experiment with publishing some
packages before making them available to the whole wide world, or there's just
some packages you don't want to expose, read on!

**The package repository website is in extreme alpha! It is very ugly and it
will change substantially! Do not use this for any critical packages! All
package and user data will be deleted sometime past Jun 15th, 2024 at midnight
UTC time. A deployment of a new server version will take place then.**

From its inception, Zokka has had the ability to use custom packages from custom
locations. However, apart from single ZIP files, it was actually pretty hard in
practice to set up your own custom package repository, even if it was possible
in theory. Even though the standard Elm package repository is open source, it
doesn't actually compile out of the box (some modifications are first required)
and it's tied to (publicly available) GitHub so that you can end up with a
problem if you have packages you'd like to eventually publish to the standard
Elm package, or if you don't want to use GitHub.

This release of Zokka finally creates a new kind of repository that is different
from the standard Elm and Zokka package repositories. These are repositories
that:

1. Are not tied to GitHub or even git
2. Are aimed at teams and individuals with private packages that are not meant
   for the external world to consume. This can either be code meant to be
   private indefinitely, or alpha/beta releases you'd like to only share with a
   small group of people first
3. Are gated behind private API keys
4. Are easily created and destroyed in a few clicks on a third-party website.
   This allows for quick and easy experimentation of package publishing instead
   of committing to publishing a packag forever on the standard Elm package
   repository.

   In particular I
   have an instance of this package server running that anyone can create an
   account on and upload their own packages gated behind an API key only they
   have access to. Read on for more information about that.

*I emphasize again that these custom repositories are meant to be private
repositories shared with a team or a small group of individuals.* These are not
meant to be repositories shared with the entire external world! That means that
every package uploaded to this repository *must* have an API key used to
authenticate it. There are no globally readable packages here!

This is because:

1. I don't want to fragment the package community. If possible, if you have some
   package you'd like to share with the world please upload it to the standard
   Elm package repository. If for some reason that is not possible, reach out to
   me and we can talk about whether it would be possible to put the package on
   the standard Zelm package repository.
2. I don't want to be on the hook for a large bandwidth bill for a free service
   because of some third-party CI process repeatedly downloading large ZIP files
   over and over again.
3. I'm allowing for deletion of repositories and packages and I believe small
   teams and individuals can handle deletions more gracefully than the
   dependency chaos that would cause for the external world at large to depend on a
   package and have it deleted out from under them.

To emphasize that this package service is not meant to be an authoritative
source for globally readable packages, I purposefully have *not* used the usual
`zokka-lang.com` domain name for this package server, but have made it on
another domain that I use for various other projects I run. In particular the
package server is located at
[https://zokka-custom-repos.igneousworkshop.com](https://zokka-custom-repos.igneousworkshop.com).

The package server to some extent, but especially the UI are *very* unpolished
and *very* ugly.  I've taken the work the work that @rlefevre did with
[https://github.com/dmy/elm.dmy.fr/](https://github.com/dmy/elm.dmy.fr/) and
butchered it with all sorts of unstyled buttons and blobs of text.

However, all the core functionality should be there. If you're willing to hold
your nose a little bit at the ugliness, I hope that you'll find this package
server useful!

This is also the first beta release of Zokka because at this point, I don't
expect the configuration file format or user interface of the Zokka compiler to
change. The package server and its UI may still change fairly drastically, but
the API with the Zokka compiler should be unchanged. This same Zokka compiler
version should work even with new versions of the package server coming in a few
months.

## Note before using the custom package manager

If you have used any version of Zokka in the past, make sure that you have the
latest version of Zokka. You should also delete your `$ELM_HOME/0.19.1/zokka`
and `$ELM_HOME/0.19.1/zokka-cache-0.191.0` folders if they exist and were
created by an older version of Zokka.

If you are using `npx` for Zokka, make sure that the `npm` version is
`0.191.0-beta.0` or later.

**Again: Do not use this for any critical packages! All packages will be deleted
sometime past Jun 15th, 2024 at midnight UTC time. A deployment of a new server
version will take place then.**

**After Jun 15th, 2024, I will likely institute pretty strict upload, download,
and storage limits!** Roughly speaking I'm imagining something like a per-user
limit of a total of 100 packages (different versions of a package each count
separately), a total of 10 MB in storage of ZIP files, and some download limit
of around 1 GB per month and an upload limit of around 100 MB per month. These
are very conservative and I will likely raise caps over time, but since now we
need to store all the data associated with a package, I don't want to wake up
one morning to a high server bill. (Note: if you're willing to pay, we can talk
about setting up a custom server for your team, or you can set up a custom
server yourself using the binaries that are published on the project GitHub
page).

## Quick start steps

1. First thing we'll do is set up everything on 
   [https://zokka-custom-repos.igneousworkshop.com](https://zokka-custom-repos.igneousworkshop.com)
   so that we have a working remote repository. You'll need to make an account the first time you navigate to the page. Note
   that this part will likely change in the coming weeks to instead use Auth0
   which provides things such as password recovery, OAuth, etc. (I also would
   prefer not to store things like email addresses on my own servers if at all
   possible).
2. Then you'll need to create a repository. Click the "Create new repository
   button" to create a repository. Give it both a human readable name (one which
   can contain any Unicode characters) and a URL safe name (characters will be
   automatically percent encoded if necessary) which will form part of the URL
   that the Zokka compiler uses to hit this repository.

   **Note that right now
   neither name is actually used anywhere. For now we just use the database ID, but
   that will change soon.** The URL safe name, since it eventually is meant to
   form part of the URL used by the Zokka compiler, must be globally unique
   (if it is not the frontend will not have a good time at the moment. That will be
   fixed.)
3. Once you've created the repository copy the JSON fragment given in "How to
   use this repository" into
   
   ```$ELM_HOME/0.19.1/zokka/custom-package-repository-config.json```

   If this file
   does not yet exist, run `npx zokka make` in any project you have to
   automatically generate this file. You can fill in `$SOME_LOCAL_NAME` with
   whatever name you want. This is the name that ultimately you will use for `zokka
   publish`. We'll use `my-custom-repo` for now. We'll fill in
   `$FULL_TOKEN_VALUE` in just a bit.
4. Create an API token by clicking "API Auth Tokens" and then clicking "Create
   new auth token." For our purposes, we'll want to create a token with package
   publish permissions.
5. Copy the token value that is displayed into your
   `$ELM_HOME/0.19.1/zokka/custom-package-repository-config.json` file. Now fill
   in `$FULL_TOKEN_VALUE` with the token value you got.
6. You can now publish to and read from your custom repository! You can run `git
   clone https://github.com/Zokka-Dev/zokka-example-package` for an example
   package which you can publish right away to your custom repository via 

   ```
   cd zokka-example-package
   npx zokka publish my-custom-repo
   ```
   (or whatever name you used for `$SOME_LOCAL_NAME` instead of
   `my-custom-repo`).

   Then in whatever other project you have you can run `npx
   zokka install example-zokka-author/example-zokka-package` and use the amazing
   new `Example.someConstant` value!
