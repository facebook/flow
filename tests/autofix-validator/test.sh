#!/bin/bash

assert_ok "$FLOW" autofix insert-type --in-place simplify-empty.js 3 28
echo "> cat simplify-empty.js"
cat simplify-empty.js
assert_ok "$FLOW" force-recheck simplify-empty.js
echo "> flow status"
assert_ok "$FLOW" status
