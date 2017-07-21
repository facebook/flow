#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" get-imports --strip-root --json a.js
assert_ok "$FLOW" get-imports --strip-root --json b
assert_ok "$FLOW" get-imports --strip-root --json b.js
assert_ok "$FLOW" get-imports --strip-root --json c.js
