#!/bin/bash
. ../assert.sh
FLOW=$1

echo "simple ES6 exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 3 17

echo

echo "shadowing an export:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 10 15

