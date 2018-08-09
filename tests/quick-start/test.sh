#!/bin/bash
printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy --file-watcher none .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nChange @flow file, expect error.\n"
echo "// change" >> a.js
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
