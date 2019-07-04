#!/bin/bash

printf "Get-def imported class:\n"
assert_ok "$FLOW" get-def bar.js 5 23 --strip-root --pretty

printf "Get-def class member:\n"
assert_ok "$FLOW" get-def bar.js 7 6 --strip-root --pretty

printf "Find-refs class property:\n"
assert_ok "$FLOW" find-refs --global foo.js 4 4 --strip-root --pretty

printf "Autocomplete class property:\n"
assert_ok "$FLOW" autocomplete bar.js 7 5 --strip-root --pretty < bar.js.txt
