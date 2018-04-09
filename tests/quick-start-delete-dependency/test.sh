#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

mkdir tmp

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy --file-watcher none .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nDelete @flow file with a @flow dependent file, expect error.\n"
mv a.js tmp/
assert_ok "$FLOW" force-recheck a.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRevert file, expect no errors.\n"
mv tmp/a.js .
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start .

rm -rf tmp
printf "\nDone!\n"
