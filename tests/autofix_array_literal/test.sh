#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

test_file(){
  FILE=$1
  echo "> insert-type" "$@"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

test_file a.js 6 15
test_file b.js 6 15 --strategy=generalize
test_file c.js 6 15 --strategy=specialize

echo "> flow status"
assert_errors "$FLOW" status --strip-root
