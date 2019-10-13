#!/bin/bash

# Run with classic
assert_errors "$FLOW" check --show-all-errors --strip-root > classic-errors.log
cat classic-errors.log

# Run with types-first
assert_errors "$FLOW" check --types-first --show-all-errors --strip-root > types-first-errors.log

assert_one() {
  assert_exit_on_line "${BASH_LINENO[0]}" "1" "$@"
}

printf "==== DIFF BETWEEN CLASSIC AND TYPES-FIRST =====\n"
# TODO This needs to be empty
assert_one diff classic-errors.log types-first-errors.log > diff.log
cat diff.log
