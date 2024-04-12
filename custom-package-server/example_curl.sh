#!/usr/bin/env bash
#
curl -X POST "http://localhost:3000/register?name=some-author/some-project&version=some-version"
curl "http://localhost:3000/all-packages"
