#!/bin/bash -e

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
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:make_js\nBuilding flow.js\n"
make js
printf "travis_fold:end:make_js\n"

printf "travis_fold:start:flow_check\nRunning flow check\n"
bin/flow check
printf "travis_fold:end:flow_check\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
FLOW_RUNTESTS_PARALLELISM=4 ./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"

printf "travis_fold:start:run_tool_test\nRunning tool test\n"
./tool test | cat # Force no tty mode
printf "travis_fold:end:run_tool_test\n"

printf "travis_fold:start:run_parser_tests\nRunning parser tests\n"
(cd src/parser && make test)
printf "travis_fold:end:run_parser_tests\n"

printf "travis_fold:start:make_ocp-build\nBuilding flow with ocp-build\n"
make build-flow-with-ocp
printf "travis_fold:end:make_ocp-build\n"
