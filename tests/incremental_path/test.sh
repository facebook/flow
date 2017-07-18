#!/bin/bash
. ../assert.sh
FLOW=$1
mkdir tmp
assert_errors "$FLOW" status .
mv dir/node_modules tmp/
assert_ok "$FLOW" force-recheck dir/node_modules/*.js
assert_errors "$FLOW" status .
mv tmp/node_modules dir/
assert_ok "$FLOW" force-recheck dir/node_modules/*.js
assert_errors "$FLOW" status .
rmdir tmp
