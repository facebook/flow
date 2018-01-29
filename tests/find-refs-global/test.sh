#!/bin/bash
. ../assert.sh
FLOW=$1

echo "simple ES6 exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 3 17

echo

echo "shadowing an export:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 10 15

echo

echo "class method:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 15 3

echo "class method, queried from a use instead of the definition:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-2.js 16 4

echo "class method that overrides a superclass implementation:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root extendsFoo.js 7 3
