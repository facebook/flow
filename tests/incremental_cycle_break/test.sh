#!/bin/bash
. ../assert.sh
FLOW=$1
mkdir tmp

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start .
cp A.js tmp/

printf "\nClear A.js:\n"
cp tmp1/A.js A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore A.js:\n"
mv tmp/A.js A.js
assert_ok "$FLOW" force-recheck --no-auto-start A.js
assert_ok "$FLOW" status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
