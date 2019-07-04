#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 6 3 6 14
assert_ok "$FLOW" autofix insert-type --in-place a.js 7 3 7 11
echo "> cat a.js"
cat a.js

assert_ok "$FLOW" autofix insert-type --in-place b.js 4 6
echo "> cat b.js"
cat b.js

assert_ok "$FLOW" autofix insert-type --in-place c.js 4 3 4 24
assert_ok "$FLOW" autofix insert-type --in-place c.js 9 17
echo "> cat c.js"
cat c.js

assert_ok "$FLOW" force-recheck {a,b,c}.js
echo "> flow status"
assert_ok "$FLOW" status
