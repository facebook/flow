#!/bin/bash

# Dump the generated file in a temporary folder and in the end Flow check them
TEMP_DIR=tmp

do_file() {
  file=$1
  echo "$file"
  assert_ok "$FLOW" autofix suggest --strip-root --quiet "$file" | tee $TEMP_DIR/"$file"
}

mkdir $TEMP_DIR
do_file "func-0.js"
do_file "func-1.js"
do_file "func-2.js"
do_file "func-3.js"

"$FLOW" init $TEMP_DIR
assert_ok "$FLOW" check $TEMP_DIR
