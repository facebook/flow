#!/bin/bash

assert_ok "$FLOW" autofix exports --in-place a.js
echo "> cat a.js"
cat a.js
assert_ok "$FLOW" force-recheck a.js
echo "> flow status"
assert_ok "$FLOW" status
