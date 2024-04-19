#!/usr/bin/env bash
#
curl -H "Authorization: CustomZokkaRepoAuthToken test-token" -F "elm.json=@test-data/example-elm.json" -F "docs.json=@test-data/example-docs.json" -F "README.md=@test-data/example-readme.md" -F "package.zip=@test-data/example-package.txt"  "http://localhost:3000/0/upload-package?name=some-author/some-project&version=some-version"
curl -H "Authorization: CustomZokkaRepoAuthToken test-token" "http://localhost:3000/0/all-packages"
