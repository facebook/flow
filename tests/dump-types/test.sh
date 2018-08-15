#!/bin/bash
assert_ok "$FLOW" dump-types --strip-root --json --pretty test.js

echo "=== predicates.js ==="
assert_ok "$FLOW" dump-types --strip-root --json --pretty predicates.js
