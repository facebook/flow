#!/bin/bash
mkdir tmp
cp root.js tmp/

assert_errors "$FLOW" status .
cp tmp1/root.js ./
assert_ok "$FLOW" force-recheck root.js
assert_errors "$FLOW" status .

mv tmp/root.js ./
rmdir tmp
