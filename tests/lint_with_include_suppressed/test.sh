#!/bin/bash
. ../assert.sh
FLOW=$1

printf "Without --include-suppressed:\n"
assert_ok "$FLOW" check
printf "With --include-suppressed:\n"
assert_ok "$FLOW" check --include-suppressed
