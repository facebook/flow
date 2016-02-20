#!/bin/bash -e

case "$TRAVIS_OS_NAME" in
  osx)
    printf "Using ocaml %s\n" "$(ocaml -vnum)"
    ;;
  linux)
    export PATH="$HOME/usr/bin:$PATH"
    eval "$(opam config env)"
    printf "Using ocaml %s and opam %s\n" "$(ocaml -vnum)" "$(opam --version)"
    ;;
esac

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"
