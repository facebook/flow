#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 5 20
assert_ok "$FLOW" force-recheck a.js

echo "> cat a.js"
cat a.js

echo "> flow status"
assert_ok "$FLOW" status
