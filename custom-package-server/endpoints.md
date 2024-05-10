## GET paths:

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
+ `/packages/{author}/{project-name}/{version}/elm.json`: returns a JSON object
  that is a copy of the usual `elm.json` file in an Elm project. Note that
  `{version}` can be `latest` in which case it should be the highest version.
+ `/packages/{author}/{project-name}/{version}/endpoint.json`: returns a JSON
  object listing the location of a zipped version of the package as well as the
  SHA1 hash of the zipfile. An example is provided below. Note that the
  URL structure of the `url` does not have to have any relation to the package
  name. Note that `{version}` can be `latest` in which case it should be the
  highest version.
  ```
  { "url": "https://example.com/some-random-path/zipfile.zip"
  , "hash": "18367cfdec746ce00766c5888813d1b2b1305685"
  }
  ```
+ `/packages/{author}/{project-name}/{version}/package.zip`: A zipfile
  containing all the source code for the library. This might be ultimately
  backed by something such as S3.

## POST paths:

+ `/{repository-id}/upload-package?name={package-name}&version={version}`: A
  `multipart/form-data` request that looks something like the following (with
  `repository-id` set to `example-repository`):
  ```
  POST /example-repository/register HTTP/1.1
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
