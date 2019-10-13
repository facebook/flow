#!/bin/bash

# Run with classic
assert_errors "$FLOW" check --show-all-errors --strip-root > classic-errors.log
cat classic-errors.log

# Run with types-first
assert_errors "$FLOW" check --types-first --show-all-errors --strip-root > types-first-errors.log

printf "==== DIFF BETWEEN CLASSIC AND TYPES-FIRST =====\n"
assert_ok diff classic-errors.log types-first-errors.log > diff.log
cat diff.log
