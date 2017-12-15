#!/bin/bash

# the OS X container has a modern node, but linux doesn't.

case "$TRAVIS_OS_NAME" in
  osx)
    ;;
  *)
    source "$HOME/.nvm/nvm.sh"
    nvm install 6
    nvm use 6
esac
