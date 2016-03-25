#!/bin/bash -e

TMP=${TMPDIR:-/tmp}

case "$TRAVIS_OS_NAME" in
  linux)
    # For some reason the Linux containers start killing the tests if too many
    # tests are run in parallel. Luckily we can easily configure that here
    export FLOW_RUNTESTS_PARALLELISM=4
    ;;
esac

INSTALL_DIR="${TMP%%/}/ocaml-${OCAML_VERSION}_opam-${OPAM_VERSION}"
export PATH="$INSTALL_DIR/usr/bin:$PATH"
export OPAMROOT="$INSTALL_DIR/.opam"
eval "$(opam config env)"

printf "Using ocaml %s from %s\n  and opam %s from %s\n" \
  "$(ocaml -vnum)" "$(which ocaml)" \
  "$(opam --version)" "$(which opam)"

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"
