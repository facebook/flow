#!/bin/bash -e

EXTRA_CC_FLAGS=""
case "$TRAVIS_OS_NAME" in
  linux)
    # The linux containers don't have much memory. Let's compile with 
    # a smaller heap size
    EXTRA_CC_FLAGS="EXTRA_CC_FLAGS=-DOSS_SMALL_HH_TABLE_POWS"
    ;;
esac

PLATFORM=$(uname -s || echo unknown)
ARCH=$(uname -m || echo unknown)
INSTALL_DIR="$HOME/.flow_cache/ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}_${PLATFORM}-${ARCH}"
export PATH="$INSTALL_DIR/usr/bin:$PATH"
export OPAMROOT="$INSTALL_DIR/.opam"
eval "$(opam config env)"

printf "Using ocaml %s from %s\n  and opam %s from %s\n" \
  "$(ocaml -vnum)" "$(which ocaml)" \
  "$(opam --version)" "$(which opam)"

printf "travis_fold:start:make\nBuilding flow\n"
make $EXTRA_CC_FLAGS
printf "travis_fold:end:make\n"

printf "travis_fold:start:make_js\nBuilding flow.js\n"
make js
printf "travis_fold:end:make_js\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"

printf "travis_fold:start:runparsertests\nRunning ocaml parser tests\n"
(cd src/parser && make test-ocaml)
printf "travis_fold:end:runparsertests\n"
