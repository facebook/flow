#!/bin/bash
assert_ok "$FLOW" autofix exports --in-place defaults.js
echo "> cat defaults.js"
cat defaults.js
assert_ok "$FLOW" force-recheck defaults.js
echo "> flow status"
assert_ok "$FLOW" status
