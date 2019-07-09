#!/bin/bash

test_file () {
  local FILE=$1
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

test_file a.js 5 20 --strategy=generalize
test_file b.js 5 20 --strategy=specialize

echo "> flow status"
assert_ok "$FLOW" status
