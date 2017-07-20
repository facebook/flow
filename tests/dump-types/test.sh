#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" dump-types --strip-root --json --pretty test.js
assert_ok "$FLOW" dump-types --strip-root --raw --pretty test.js
