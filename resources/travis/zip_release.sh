#!/bin/bash -e

if [[ "$TRAVIS_TAG" = "" ]]; then exit 0; fi

PLATFORM=$([ "$TRAVIS_OS_NAME" == "linux" ] && echo "linux64" || echo "$TRAVIS_OS_NAME")

mkdir -p $HOME/release
rm -rf $HOME/release/flow
cp -R bin $HOME/release/flow
cd $HOME/release && zip -r \
  "$TRAVIS_BUILD_DIR/flow-$PLATFORM-$TRAVIS_TAG.zip" \
  flow \
  -x \*.tar.gz # exclude flowlib.tar.gz
