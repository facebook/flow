#!/bin/bash

# install system dependencies:
#
# - aspcud for opam (on OS X; installed via apt on Linux)
# - awscli (to talk to AWS)
# - modern node (for yarn and npm)
# - yarn

case "$TRAVIS_OS_NAME" in
  osx)
    printf "travis_fold:start:brew_install\nInstalling brew\n"
    brew update
    brew install aspcud awscli yarn
    printf "travis_fold:end:brew_install\n"
    ;;
  *)
    source $HOME/.nvm/nvm.sh
    nvm install 6
    nvm use 6
esac
