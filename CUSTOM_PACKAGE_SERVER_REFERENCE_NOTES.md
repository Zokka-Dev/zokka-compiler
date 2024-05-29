# Structure of the Zokka Custom Package Server

This lays out a very informal specification of the most common Zokka package
server that we'll be coding against. That is these are repositories marked as
`package-server-with-elm-package-server-read-api-and-direct-package-upload` in
`custom-package-repository-config.json`.

We depart from the usual Elm package server in two major ways:

1. Add an authentication token which is sent as a header on all HTTP requests.
2. Add an additional zipfile which is uploaded as part of the multipart HTTP
   POST request to the `/register` endpoint.

Note that while it is not necessary to discard `git` and GitHub to adhere to
this specification, the ability to directly upload zipfiles along with a
separate system for authentication does obviate their need.

Otherwise, we'll follow the standard Elm package server API pretty closely, so
our overall API endpoints are the same as the standard Elm package server.

As a point of reference, we'll first cover the standard Elm package server API
endpoints.

## Standard Elm Package Server API

### GET paths:

+ `/all-packages/`: returns a JSON object listing all packages on the server.
  An example of the structure of the JSON object is provided below.
  ```
  { "author_0/project-name": ["1.0.0", "2.0.0"]
  , "author_1/project-name": ["1.0.0", "1.0.1"]
  }
  ```
+ `/all-packages/since/{n}`: returns a JSON object listing all packages on the
  server that have been created since the `n`th package was created. An example
  of the structure of the JSON objet is provided below.
  ```
  [ "author_0/project-name@2.0.0"
  , "author_1/project-name@1.0.1"
  ]
  ```
  Note that this means that packages once created can never be deleted,
  otherwise the number of packages would change and mess up the `n` counter.
+ `/packages/{author}/{project-name}/{version}/endpoint.json`: returns a JSON
  object listing the location of a zipped version of the package as well as the
  SHA1 hash of the zipfile. An example is provided below. Note that the
  URL structure of the `url` does not have to have any relation to the package
  name.
  ```
  { "url": "https://example.com/some-random-path/zipfile.zip"
  , "hash": "18367cfdec746ce00766c5888813d1b2b1305685"
  }
  ```

### POST paths:

+ `/register?name={package-name}&version={version}&commit-hash={commit-hash}`: A
  `multipart/form-data` request that looks something like the following:
  ```
  POST /register HTTP/1.1
  Host: foo.example
  Content-Type: multipart/form-data;boundary="boundary"

  --boundary
  Content-Disposition: form-data; name="elm.json"; filename="elm.json"

  {"type":"package",...}
  --boundary
  Content-Disposition: form-data; name="docs.json"; filename="docs.json"

  {"name":"Example",...}
  --boundary
  Content-Disposition: form-data; name="README.md"; filename="README.md"

  # This is a README...
  --boundary
  Content-Disposition: form-data; name="github-hash"

  16920ba660a22eb1a6606e1cd02a0712c2bda9e3
  --boundary--
  ```

### Server behavior for POST requests

Upon a call to `/register`, the standard Elm package server will perform the
following verification (purely using the query parameters, i.e. the `elm.json`
file that is uploaded is not used for any verification):
1. Verify that the package name, as a GitHub username and repository name, are
   in fact publically accessible on GitHub
2. Verify that the package version exists as a tag in the GitHub repo
3. Verify that the package version has not been published previously to the
   package server
4. Verify that the commit hash provided as a query parameter matches the commit
   hash of the GitHub tag whose name is the same as the version number of the
   package

## Differences for package server with direct package upload

For our
`package-server-with-elm-package-server-read-api-and-direct-package-upload`,
we're going require a header of `repository-auth-token` for every HTTP request.
The `GET` paths will otherwise stay the same.

Our `POST` request to `/register` will change. It will omit all mention of
git/GitHub commit hashes and will add an additional `package.zip` which is meant
to be the zipfile containing the entire package.

To emphasize that the format of our upload request has changed, the endpoint
name is also changed, from `register` to `upload-package`.

+ `/upload-package?name={package-name}&version={version}`: A
  `multipart/form-data` request that looks something like the following:
  ```
  POST /upload-package HTTP/1.1
  Host: foo.example
  Content-Type: multipart/form-data;boundary="boundary"

  --boundary
  Content-Disposition: form-data; name="elm.json"; filename="elm.json"

  {"type":"package",...}
  --boundary
  Content-Disposition: form-data; name="docs.json"; filename="docs.json"

  {"name":"Example",...}
  --boundary
  Content-Disposition: form-data; name="README.md"; filename="README.md"

  # This is a README...
  --boundary
  Content-Disposition: form-data; name="package.zip"; filename="package.zip"

  ....
  --boundary--
  ```

We will also get rid of the `since` endpoint. We will allow users to delete
custom repositories and packages, which significantly complicates how we do
incremental package list updates. We cannot simply use a single numerical offset
since that only works for append-only data structures. There are ideas I have to
re-introduce incremental package list updates (mainly turning things into events
and transmitting both "new package" and "package deleted" events from an
offset), but they aren't necessary for this first iteration to work. The
*entire* Elm package index is 205 KB, which is only 44 KB gzipped. Asking a user
to simply redownload the entire package index of their custom repository each
time will likely be fine for a pretty long time to come.

Deletion of packages will be necessary in part because we will likely institute
restrictions on the number of packages any single user to reduce server costs
since we will now be storing the packages ourselves and could balloon our
storage costs if not careful. Thus a user could be indefinitely locked out from
uploading new packages if they hit a storage limit and had no way of removing
packages.

Since these are custom repositories meant for private usage or internally within
an organization, deleting a package or repository is far less impactful than if
this was a globally available repository.

### UI paths

There are also paths which exist for the web UI, entirely under the `/dashboard`
path. These are paths which will never be hit by the Zokka compiler when
publishing or downloading a package, but are necessary for a user to manage
authentication tokens and repositories. I won't list them here because they are
more subject to change and can evolve independently of the Zokka compiler.

### Design

The original Elm package website creates a directory of files on disk and uses
that entirely for its persistent state, i.e. it is not backed by a database.
On the one hand this is quite nice for making sure that we can reduce a lot of
requests down to static file serving which minimizes computational load on the
server.

On the other hand, it tightly couples the package website's design to a single
filesystem and makes it difficult to e.g. serve packages from S3 or some sort of
key-value store that doesn't support efficient enumeration. It also somewhat
complicates backups. There are solutions that allow for us to live-replicate a
DB to ensure we never lose any data. Those solutions seem to be a bit trickier
for filesystems (most I've seen seem to replicate on some predetermined
schedule).

In order to do per-user authentication (something we'll need to implement since
we're dropping the GitHub dependence), we'll need some sort of database anyway.

As such, I've decided to code up the package server from scratch. There are few
enough API endpoints that it's a feasible task and the moving away from the
elm-package server is not a ridiculous burden.

As a first pass I'll use SQLite as the backing database. We have a comparatively
write-light workload, so the sequential write restriction that SQLite has will
likely not be a big deal.

Moreover SQLite has its `sqlar` module, which allows us to treat SQLite also as a
filestore, so we can directly store ZIP blobs within our SQLite database itself,
reducing our need for persistence and all the attendent complications with
persistence (backups, atomicity, consistency, etc.) to a single point: the
SQLite file.

We will likely still want to keep the `sqlar` part of SQLite fairly separate
from the rest of the database (i.e. not try to extend the standard `sqlar`
table), to allow us to stay flexible with moving this out eventually if it turns
out we simply have too many blobs to store on the server itself and want to move
out to e.g. S3.
