#!/bin/bash
assert_ok "$FLOW" autofix insert-type --in-place a.js 3 12 3 14
assert_ok "$FLOW" autofix insert-type --in-place a.js 5 12 5 14
assert_ok "$FLOW" autofix insert-type --in-place a.js 8 12 8 14

cat a.js
assert_ok "$FLOW" force-recheck a.js
assert_ok "$FLOW" status --strip-root
