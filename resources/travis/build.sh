#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/setup_opam.sh

printf "Using ocaml %s from %s\n  and opam %s from %s\n" \
  "$(ocaml -vnum)" "$(which ocaml)" \
  "$(opam --version)" "$(which opam)"

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

printf "travis_fold:start:libflowparser\nBuilding libflowparser\n"
make -C src/parser dist/libflowparser.zip
printf "travis_fold:end:libflowparser\n"

printf "travis_fold:start:make_js\nBuilding flow.js\n"
make js
printf "travis_fold:end:make_js\n"

printf "travis_fold:start:make_ocp-build\nBuilding flow with ocp-build\n"
make all-ocp
printf "travis_fold:end:make_ocp-build\n"

printf "travis_fold:start:flow_check\nRunning flow check\n"
bin/flow check packages/flow-dev-tools
printf "travis_fold:end:flow_check\n"

printf "travis_fold:start:test_tool\nRunning tests for tool\n"
# FLOW_BIN is relative to packages/flow-dev-tools
FLOW_BIN=../../bin/flow make -C packages/flow-dev-tools test
printf "travis_fold:end:test_tool\n"

printf "travis_fold:start:runtests\nRunning flow tests\n"
FLOW_RUNTESTS_PARALLELISM=4 ./runtests.sh bin/flow
printf "travis_fold:end:runtests\n"

printf "travis_fold:start:run_tool_test\nRunning tool test\n"
./tool test | cat # Force no tty mode
printf "travis_fold:end:run_tool_test\n"

printf "travis_fold:start:run_parser_tests\nRunning parser tests\n"
(cd src/parser && make test)
make test-parser-ocp
printf "travis_fold:end:run_parser_tests\n"
