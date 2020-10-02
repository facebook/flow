#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
start_flow tmp

do_file () {
  FILE=$1; shift

  cp "$FILE" "tmp/$FILE"

  while [[ $# -ge 2 ]]; do
    local a1=$1; shift;
    local a2=$1; shift;
    echo "> flow autofix insert-type $FILE $a1 $a2"
    assert_ok "$FLOW" autofix insert-type --in-place "$FILE" "$a1" "$a2"
  done
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

do_file a.js 3 20
do_file b.js 3 25

echo "> flow status"
assert_ok "$FLOW" status
echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
