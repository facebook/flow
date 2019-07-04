#!/bin/bash
assert_ok "$FLOW" autofix insert-type --in-place a.js 9 18 9 32
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status
