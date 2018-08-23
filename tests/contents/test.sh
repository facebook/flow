#!/bin/bash
assert_ok "$FLOW" get-def --strip-root --json ignore/test.js 3 2
assert_ok "$FLOW" type-at-pos --strip-root --json ignore/test.js 3 2
assert_ok "$FLOW" get-def --strip-root --json no_flow/test.js 3 2
assert_ok "$FLOW" type-at-pos --strip-root --json no_flow/test.js 3 2
