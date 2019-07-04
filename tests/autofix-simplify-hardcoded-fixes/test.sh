#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place a.js 5 18 5 21
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status --strip-root
