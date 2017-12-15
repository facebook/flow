#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nTouch @flow file, expect error.\n"
touch a.js
assert_ok "$FLOW" force-recheck a.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nMake file @noflow, expect error to go away.\n"
cp tmp1/a.js a.js
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nRevert file, expect error again.\n"
cp tmp2/a.js a.js
assert_ok "$FLOW" force-recheck a.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nDone!\n"
