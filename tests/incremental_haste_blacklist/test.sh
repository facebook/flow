#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nInitial status:\n"
assert_ok $FLOW status --no-auto-start --strip-root .

printf "\nMove A.js to B.js (blacklisted):\n"
mv A.js B.js
assert_ok $FLOW force-recheck --no-auto-start A.js B.js
assert_errors $FLOW status --no-auto-start --strip-root .

printf "\nMove B.js to A.js:\n"
mv B.js A.js
assert_ok $FLOW force-recheck --no-auto-start A.js B.js
assert_ok $FLOW status --no-auto-start --strip-root .

printf "\nDone!\n"
