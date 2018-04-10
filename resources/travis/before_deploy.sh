#!/bin/bash -e

if [[ "$TRAVIS_TAG" = "" ]]; then exit 0; fi

# We don't deploy the whole website on tags, but we do upload flow.js and the
# libs. This sets up the directory we'll upload. See the deploy section of
# .travis.yml.
mkdir -p "$HOME/static/$TRAVIS_TAG"
cp "bin/flow.js" "$HOME/static/$TRAVIS_TAG/flow.js"
cp -r "lib" "$HOME/static/${TRAVIS_TAG}/flowlib"
