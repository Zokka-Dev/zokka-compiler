#!/usr/bin/env bash
#
curl -F "elm.json=@test-data/example-elm.json" -F "docs.json=@test-data/example-docs.json" -F "README.md=@test-data/example-readme.md" -F "package.zip=@test-data/example-package.txt"  "http://localhost:3000/register?name=some-author/some-project&version=some-version"
curl "http://localhost:3000/all-packages"
