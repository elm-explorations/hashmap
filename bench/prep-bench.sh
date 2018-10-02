#!/bin/sh

set -e

../node_modules/.bin/elm-make --yes --output index.html Main.elm
