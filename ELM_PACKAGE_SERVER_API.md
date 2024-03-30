# Structure of the Elm Package Server

This lays out the overall structure of a package server that we'll be coding
against. We'll follow the standard Elm package server API exactly for GET
paths.

## GET paths:

+ `/all-packages/`: returns a JSON object listing all packages on the server.
  An example of the structure of the JSON object is provided below.
  ```
  { "author_0/package-name": ["1.0.0", "2.0.0"]
  , "author_1/package-name": ["1.0.0", "1.0.1"]
  }
  ```
+ `/all-packages/since/{n}`: returns a JSON object listing all packages on the
  server that have been created since the `n`th package was created. An example
  of the structure of the JSON objet is provided below.
  ```
  [ "author_0/package-name@2.0.0"
  , "author_1/package-name@1.0.1"
  ]
  ```
  Note that this means that packages once created can never be deleted,
  otherwise the number of packages would change and mess up the `n` counter.
+ `/packages/{author}/{package-name}/{version}/endpoint.json`: returns a JSON
  object listing the location of a zipped version of the package as well as the
  SHA1 hash of the zipfile.  An example is provided below.
  ```
  { "url": "https://example.com/some-random-path/zipfile.zip"
  , "hash": "18367cfdec746ce00766c5888813d1b2b1305685"
  }
  ```
