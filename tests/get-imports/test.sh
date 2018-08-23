#!/bin/bash
assert_ok "$FLOW" get-imports --strip-root --pretty a.js
assert_ok "$FLOW" get-imports --strip-root --pretty b
assert_ok "$FLOW" get-imports --strip-root --pretty b.js
assert_ok "$FLOW" get-imports --strip-root --pretty c.js
