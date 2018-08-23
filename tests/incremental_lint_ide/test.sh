#!/bin/bash -e
assert_ok "$FLOW" force-recheck --no-auto-start a.js
assert_ok "$FLOW" status --no-auto-start .

cp tmp/b.js ./
trap 'rm b.js' EXIT

assert_ok "$FLOW" force-recheck --no-auto-start b.js
assert_errors "$FLOW" status --no-auto-start .
