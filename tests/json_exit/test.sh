#!/bin/bash
. ../assert.sh
FLOW=$1

assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" \
  "$FLOW" check --json pants

assert_exit "$EXIT_COULD_NOT_FIND_FLOWCONFIG" \
  "$FLOW" check --pretty pants

assert_exit "$EXIT_USAGE" \
  "$FLOW" check --json --pants

assert_exit "$EXIT_USAGE" \
  "$FLOW" check --pretty --pants
