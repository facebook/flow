#!/bin/bash -e

if [[ "$TRAVIS_TAG" = "" ]]; then exit 0; fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/setup_opam.sh

PLATFORM=$([ "$TRAVIS_OS_NAME" == "linux" ] && echo "linux64" || echo "$TRAVIS_OS_NAME")

cp src/parser/dist/libflowparser.zip \
  "$TRAVIS_BUILD_DIR/libflowparser-$PLATFORM-$TRAVIS_TAG.zip"

make dist/flow.zip
cp dist/flow.zip "$TRAVIS_BUILD_DIR/flow-$PLATFORM-$TRAVIS_TAG.zip"

# We don't deploy the whole website on tags, but we do upload flow.js and the
# libs. This sets up the directory we'll upload. See the deploy section of
# .travis.yml.
if [ "$TRAVIS_OS_NAME" == "linux" ]; then
  mkdir -p "$HOME/static/$TRAVIS_TAG"
  cp "bin/flow.js" "$HOME/static/$TRAVIS_TAG/flow.js"
  cp -r "lib" "$HOME/static/${TRAVIS_TAG}/flowlib"
fi
