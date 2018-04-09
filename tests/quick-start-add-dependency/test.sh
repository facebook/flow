#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy --file-watcher none .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nTouch @flow file with dependency on a @flow file, expect no error.\n"
touch b.js
assert_ok "$FLOW" force-recheck b.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
