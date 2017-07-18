#!/bin/sh
. ../assert.sh
FLOW=$1

printf "\nStop any already-running server."
assert_ok "$FLOW" stop .

printf "\nFull check:\n"
assert_errors "$FLOW" check .

printf "\nFocused check on a file with local errors but no dependency or reverse dependency:\n"
assert_errors "$FLOW" focus-check test.js

printf "\nFocused check on a file with no local errors but a dependency:\n"
assert_errors "$FLOW" focus-check a.js

printf "\nFocused check on a file with no local errors but a reverse dependency:\n"
assert_errors "$FLOW" focus-check b.js

printf "\nDone!\n"
