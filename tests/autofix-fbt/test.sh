#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 6 18 6 23
assert_ok "$FLOW" force-recheck a.js

echo "> cat a.js"
cat a.js
echo "> flow status"
assert_ok "$FLOW" status
