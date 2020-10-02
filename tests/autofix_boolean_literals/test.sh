#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
start_flow tmp

test_file () {
  local FILE=$1
  cp "$FILE" "tmp/$FILE"

  echo "> autofix insert-type" "$@"
  assert_ok "$FLOW" autofix insert-type --in-place "$@"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

test_file a.js 5 20 --strategy=generalize
test_file b.js 5 20 --strategy=specialize

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
