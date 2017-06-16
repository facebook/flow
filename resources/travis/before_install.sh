#!/bin/bash

# install system dependencies:
#
# - aspcud for opam (on OS X; installed via apt on Linux)
# - awscli (to talk to AWS)
# - modern node (for yarn and npm)
# - yarn

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case "$TRAVIS_OS_NAME" in
  osx)
    printf "travis_fold:start:brew_install\nInstalling brew\n"
    brew update
    brew install aspcud awscli yarn
    printf "travis_fold:end:brew_install\n"
    ;;
  *)
    ;;
esac

source "$DIR/setup_node.sh"
