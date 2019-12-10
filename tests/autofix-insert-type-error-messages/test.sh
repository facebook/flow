#!/usr/bin/env bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

UNKNOWN=110

# Note when the grep fails error output is suppressed
# I usually just remove the error output greping and writing to the .err file (using 2 > &1 instead)
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
# Fails because the user could want an exact object or the general object
test_file "$UNKNOWN" object.js 4 8 --strategy=fail

test_file "$UNKNOWN" empty-array.js 3 12 3 14

# Type isn't well scoped
test_file "$UNKNOWN" type-shadowing.js 4 41 4 42

# Type isn't representatable in contrete syntax
test_file "$UNKNOWN" recursive.js 3 21
test_file "$UNKNOWN" anonymous.js 3 13
