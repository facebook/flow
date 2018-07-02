#!/bin/bash
. ../assert.sh
FLOW=$1

printf "\nInitial status:\n"
assert_errors "$FLOW" status --no-auto-start .

printf "\nCreate data.json:\n"
cp data.json1 data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_ok "$FLOW" status --no-auto-start .

printf "\nModify data.json:\n"
cp data.json2 data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_errors "$FLOW" status --no-auto-start .

printf "\nDelete data.json:\n"
rm data.json
assert_ok "$FLOW" force-recheck --no-auto-start data.json
assert_errors "$FLOW" status --no-auto-start .

printf "\nDone!\n"
