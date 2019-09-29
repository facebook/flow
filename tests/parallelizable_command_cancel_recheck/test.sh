#!/bin/bash

printf "\nNo errors thanks to lazy mode:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root

printf "\nAutocomplete should kick off a recheck:\n"
assert_ok "$FLOW" autocomplete --strip-root --wait-for-recheck false \
  focused.js 5 3 < focused.js.stdin

printf "\nNow we should see the errors:\n"
assert_errors "$FLOW" status --no-auto-start --strip-root
