#!/bin/sh
FLOW=$1

printf "Without --include-suppressed\n"
"$FLOW" check --all --strip-root

printf "\n\nWith --include-suppressed\n"
"$FLOW" check --all --strip-root --include-suppressed

printf "\n\nJSON without --include-suppressed\n"
"$FLOW" check --all --strip-root --json --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n\nJSON with --include-suppressed\n"
"$FLOW" check --all --strip-root --include-suppressed --json --pretty \
  | grep -v '^ *"flowVersion":.*'

printf "\n"
