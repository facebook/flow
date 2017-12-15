#!/bin/bash
. ../assert.sh
FLOW=$1
mkdir tmp
printf "\nShould resolve to dir/node_modules/b.js which is number\n"
assert_errors "$FLOW" status .
printf "\nShould resolve to node_modules/b.js which is a string\n"
mv dir/node_modules tmp/
assert_ok "$FLOW" force-recheck dir/node_modules/*.js
assert_errors "$FLOW" status .
printf "\nShould resolve to dir/node_modules/b.js which is number\n"
mv tmp/node_modules dir/
assert_ok "$FLOW" force-recheck dir/node_modules/*.js
assert_errors "$FLOW" status .
rmdir tmp
