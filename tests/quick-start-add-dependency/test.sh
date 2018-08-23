#!/bin/bash
printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
start_flow . --lazy

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nChange @flow file with dependency on a @flow file, expect no error.\n"
echo "// change" >> b.js
assert_ok "$FLOW" force-recheck b.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
