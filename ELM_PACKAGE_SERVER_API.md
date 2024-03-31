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

As a point of reference, we'll first the standard Elm package server API
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

+ `/register?name={package-name}&version={version}`: A
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
  Content-Disposition: form-data; name="package.zip"; filename="package.zip"

  ....
  --boundary--
  ```
