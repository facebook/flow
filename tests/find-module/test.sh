#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok "$FLOW" find-module --strip-root --json ./req test.js
assert_ok "$FLOW" get-def --strip-root --json test.js 2 2
assert_ok "$FLOW" get-def --strip-root --json test.js 1 10
assert_ok "$FLOW" get-def --strip-root --json test.js 4 18
