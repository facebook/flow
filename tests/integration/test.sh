#!/bin/bash
. ../assert.sh
FLOW=$1

mv bar.js _bar.js
assert_ok $FLOW force-recheck --root . bar.js _bar.js
assert_errors $FLOW status .
mv _bar.js bar.js
