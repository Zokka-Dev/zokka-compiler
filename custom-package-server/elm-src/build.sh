#!/usr/bin/env bash
#
elm make frontend/Main.elm --output=elm.js

mkdir -p ../generated-static-files/ui
cp elm.js ../generated-static-files/ui
cp index.html ../generated-static-files/ui
cp -r assets ../generated-static-files/ui
