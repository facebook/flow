#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nStop any already-running server.\n"
assert_ok "$FLOW" stop .

printf "\nQuick start.\n"
assert_ok "$FLOW" start --lazy --file-watcher none .

printf "\nExpect no errors.\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\n\"Warm up\" a dependency.\n"
touch b.js
assert_ok "$FLOW" force-recheck b.js
assert_ok "$FLOW" status --no-auto-start .

printf "\n\"Perturb\" a dependent that depends on the warmed up, but unchanged dependency.\n"
touch e.js
assert_ok "$FLOW" force-recheck e.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nTest that the perturbed dependent is actually known (expecting a crash otherwise).\n"
touch d.js
assert_ok "$FLOW" force-recheck d.js
assert_ok "$FLOW" status --no-auto-start .

printf "\nDone!\n"
