#!/bin/bash
. ../assert.sh

FLOW=$1

printf "\nStop any already-running server.\n"
assert_ok $FLOW stop .

printf "\nQuick start.\n"
assert_ok $FLOW start --lazy .

printf "\nExpect no errors.\n"
assert_ok $FLOW status --no-auto-start

printf "\nCheck contents with dependency, expect errors in contents.\n"
assert_errors $FLOW check-contents --no-auto-start test.js < test.js

printf "\nExpect errors in dependency.\n"
assert_errors $FLOW status --no-auto-start

printf "\nUnsaved files don't have dependents, as in non-lazy mode.\n"
assert_ok $FLOW check-contents --no-auto-start foo.js < unsaved_foo.js

printf "\nDone!\n"
