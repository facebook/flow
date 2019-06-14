#!/bin/bash

## Correct tests

# Dump the generated file in a temporary folder and in the end Flow check them
TEMP_DIR=tmp

do_file() {
  file=$1;
  line=$2;
  column=$3;
  echo "$file $line $column"
  assert_ok "$FLOW" autofix insert-type --strip-root --quiet "$file" "$line" "$column"| tee $TEMP_DIR/"$file"
}

mkdir $TEMP_DIR
do_file "array.js" 3 15
do_file "arrow-0.js" 3 8
do_file "arrow-0.js" 3 13
do_file "arrow-0.js" 3 14
do_file "arrow-1.js" 3 16
do_file "arrow-2.js" 3 5
do_file "class-0.js" 4 4 # Need annotations for vars
do_file "class-1.js" 5 6
do_file "class-2.js" 4 6
do_file "class-3.js" 9 15
do_file "comments-0.js" 4 33
do_file "dictionary.js" 12 12 # getters and setters need return type annotations
do_file "func-0.js" 3 26
do_file "func-1.js" 3 18
do_file "func-2.js" 4 17
do_file "func-3.js" 2 22
do_file "func-4.js" 2 22
do_file "func-5.js" 2 22
do_file "func-poly-0.js" 3 24
do_file "object-0.js" 7 6
do_file "object-1.js" 7 8
do_file "object-2.js" 6 6
do_file "poly-0.js" 3 22
do_file "poly-0.js" 3 15
do_file "react-0.js" 6 21 # This returns any like suggest, but type at point {|date : Date|}
do_file "spread.js" 10 3
do_file "string-literal.js" 11 14
do_file "type-utils.js" 6 3
do_file "union-0.js" 3 15
do_file "replacement-array.js" 5 15
do_file "replacement-arrow.js" 3 13
do_file "replacement-class.js" 5 7
do_file "replacement-dictionary.js" 3 3
do_file "replacement-function.js" 7 47
do_file "replacement-object.js" 2 16

$FLOW init $TEMP_DIR
assert_ok "$FLOW" check $TEMP_DIR

assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet "object-0.js" 4 4
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet "poly-0.js" 3 21
