#!/bin/bash

printf "Get-def imported class:\n"
assert_ok "$FLOW" get-def bar.js 5 23 --strip-root --pretty

printf "Get-def class member:\n"
assert_ok "$FLOW" get-def bar.js 7 6 --strip-root --pretty
