#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 3 20
assert_ok "$FLOW" force-recheck a.js
echo "> cat a.js"
cat a.js

assert_ok "$FLOW" autofix insert-type --in-place b.js 3 25
assert_ok "$FLOW" force-recheck b.js
echo "> cat b.js"
cat b.js

echo "> flow status"
assert_ok "$FLOW" status
