#!/bin/bash

# autofix doesn't try to control the order of unions
update_in_place(){
  local FILE=$1;
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

update_in_place any-in-union.js 12 5
update_in_place any-in-union.js 13 5
update_in_place any-in-union.js 14 5
update_in_place any-in-union.js 15 5
update_in_place any-in-union.js 16 5
update_in_place any-to-flowfixme-no-strict.js 4 20
update_in_place any-to-flowfixme-strict-local.js 4 20
update_in_place any-to-flowfixme-strict.js 4 20

echo "> flow status"
assert_ok "$FLOW" status
