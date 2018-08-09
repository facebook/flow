#!/bin/bash
printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy --file-watcher none .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nChange @flow file with dependency on a @flow file in a cycle, expect no error.\n"
echo "// change" >> c.js
assert_ok "$FLOW" force-recheck c.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
