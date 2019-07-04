#!/bin/bash
assert_ok "$FLOW" autofix insert-type --in-place defaults.js 8 18 8 23
echo "> cat defaults.js"
cat defaults.js
assert_ok "$FLOW" force-recheck defaults.js
echo "> flow status"
assert_ok "$FLOW" status
