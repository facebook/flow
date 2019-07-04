#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 6 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 7 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 8 9
assert_ok "$FLOW" autofix insert-type --in-place a.js 9 9
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status
