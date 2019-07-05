#!/usr/bin/env bash

UNKNOWN=110

# Note when the grep fails error output is suppressed
# I usually just remove the error output greping and writing to the .err file
# The error grepping is useful to filter out spurious warnings
test_file () {
  local EXIT="$1"; shift;
  local FILE="$1"; shift;
  echo "$FILE"
  assert_exit "$EXIT" "$FLOW" autofix insert-type "$FILE" "$@" 2> "$FILE.err"
  # If grep fails I don't want the test to fail
  grep "flow autofix insert-type:"  "$FILE.err" || true
}

# User Errors
test_file "$UNKNOWN" empty.js
test_file "$UNKNOWN" empty.js 5 0
test_file "$UNKNOWN" annotated.js 3 6

# No perfect solution errors
# Type size bigger than expected
test_file "$UNKNOWN" big-type.js 3 6
# A concurrent diff will make this error possible
# test_file "multiple possible annotations" object.js 4 8 --strategy=fail

# Type isn't well scoped
test_file "$UNKNOWN" type-shadowing.js 4 41 4 42

# Type isn't representatable in contrete syntax
test_file "$UNKNOWN" recursive.js 3 21
test_file "$UNKNOWN" anonymous.js 3 13
