#!/bin/bash
assert_ok "$FLOW" dump-types --strip-root --json --pretty test.js

echo "=== predicates.js ==="
assert_ok "$FLOW" dump-types --strip-root --json --pretty predicates.js

echo "=== type-destructors.js ==="
assert_ok "$FLOW" dump-types --strip-root type-destructors.js | grep '^type-destructors.js:7'

echo "=== type-destructors.js (--evaluate-type-destructors) ==="
assert_ok "$FLOW" dump-types --strip-root --evaluate-type-destructors type-destructors.js | grep '^type-destructors.js:7'

echo "=== type-destructors.js (--expand-type-aliases --evaluate-type-destructors) ==="
assert_ok "$FLOW" dump-types --strip-root --expand-type-aliases --evaluate-type-destructors type-destructors.js | grep '^type-destructors.js:7'
