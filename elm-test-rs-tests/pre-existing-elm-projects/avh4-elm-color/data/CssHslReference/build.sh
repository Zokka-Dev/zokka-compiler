#!/bin/bash

set -ex

cd "`dirname "$0"`"

# Build docker image with the ABC interpretter
[ -f abc.tar.gz ] || curl -O https://homepages.cwi.nl/~steven/abc/implementations/abc.tar.gz
shasum -c shasum.txt
tar zxvf abc.tar.gz
chmod a+x ABC/abc ABC/abckeys
docker build . -t elm-color-abc

# Run the ABC script to generate CssHslReferenceData.elm
time docker run --rm -i elm-color-abc - < hsl-to-rgb.abc > ../../tests/CssHslReferenceData.elm
