#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $DIR/setup_opam.sh

printf "travis_fold:start:make\nBuilding flow\n"
make
printf "travis_fold:end:make\n"

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
