#!/bin/sh
printf "Without --include-warnings\n"
assert_errors "$FLOW" check --all --strip-root

printf "\n\nWith --include-warnings\n"
assert_errors "$FLOW" check --all --strip-root --include-warnings

printf "\n\nJSON without --include-warnings\n"
assert_errors "$FLOW" check --all --strip-root --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n\nJSON with --include-warnings\n"
assert_errors "$FLOW" check --all --strip-root --include-warnings --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n"
