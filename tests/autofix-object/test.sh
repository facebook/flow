#!/bin/bash

run_test(){
  local FILE="$1"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

run_test a.js 4 8
run_test b.js 3 15
run_test c.js 4 6

echo "> flow status"
assert_ok "$FLOW" status
