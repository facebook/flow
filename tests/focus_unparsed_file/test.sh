#!/bin/bash

printf "\nNo errors:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root

printf "\nFocusing the unparsed file shouldn't blow up:\n"
assert_ok "$FLOW" force-recheck --focus --no-auto-start unparsed.js

printf "\nNo errors:\n"
assert_ok "$FLOW" status --no-auto-start --strip-root
