#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

mkdir tmp || rm tmp/*
cp .flowconfig tmp/.flowconfig
start_flow tmp

fix_missing_local_annot() {
  FILE=$1; shift

  cp "$FILE" "tmp/$FILE"

  echo "> flow autofix missing-local-annot $FILE"
  assert_ok "$FLOW" autofix missing-local-annot --in-place "$FILE"
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

insert_type () {
  FILE=$1; shift

  cp "$FILE" "tmp/$FILE"

  while [[ $# -ge 4 ]]; do
    local a1=$1; shift;
    local a2=$1; shift;
    local a3=$1; shift;
    local a4=$1; shift;
    echo "> flow autofix insert-type $FILE $a1 $a2 $a3 $a4"
    assert_ok "$FLOW" autofix insert-type --in-place "$FILE" "$a1" "$a2" "$a3" "$a4"
  done
  assert_ok "$FLOW" force-recheck "$FILE"
  echo "> cat $FILE"
  cat "$FILE"
}

fix_missing_local_annot a.js
insert_type a.js 7 3 7 11
fix_missing_local_annot b.js
fix_missing_local_annot c.js
insert_type c.js 4 3 4 32    9 17 9 17

echo "> flow status"
assert_ok "$FLOW" status

echo "> flow status tmp"
assert_ok "$FLOW" status tmp

assert_ok "$FLOW" stop tmp
