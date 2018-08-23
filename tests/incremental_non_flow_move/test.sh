#!/bin/bash
mkdir tmp

printf "\nInitial status:\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\nDelete non-@flow file foo.js:\n"
mv foo.js tmp
assert_ok "$FLOW" force-recheck --no-auto-start foo.js
assert_errors "$FLOW" status --no-auto-start .

printf "\nRestore non-@flow file foo.js:\n"
mv tmp/foo.js .
# NOTE: force-rechecking foo.js defeats the purpose of this particular test
# "$FLOW" force-recheck --no-auto-start foo.js
assert_ok "$FLOW" status --no-auto-start .

rmdir tmp
printf "\nDone!\n"
