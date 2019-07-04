#!/bin/bash

## Correct tests

# Dump the generated file in a temporary folder and in the end Flow check them
TEMP_DIR=tmp

do_file() {
  file=$1
  echo "$file"
  assert_ok "$FLOW" suggest --strip-root --quiet "$file" | tee $TEMP_DIR/"$file"
}

mkdir $TEMP_DIR
do_file "array.js"
do_file "arrow-0.js"
do_file "arrow-1.js"
do_file "class-0.js"
do_file "class-1.js"
do_file "class-2.js"
# do_file "class-3.js" # TODO
do_file "comments-0.js"
do_file "dictionary.js"
do_file "func-0.js"
do_file "func-1.js"
do_file "func-2.js"
do_file "func-3.js"
do_file "func-4.js"
do_file "func-5.js"
do_file "func-poly-0.js"
do_file "object-0.js"
do_file "object-1.js"
do_file "object-2.js"
do_file "poly-0.js"
do_file "react-0.js"
do_file "spread.js"
do_file "string-literal.js"
do_file "type-utils.js"
do_file "union-0.js"
do_file "replacement-array.js"
do_file "replacement-arrow.js"
do_file "replacement-class.js"
do_file "replacement-dictionary.js"
do_file "replacement-function.js"
do_file "replacement-object.js"
do_file "func-arg-only.js"

"$FLOW" init $TEMP_DIR
assert_ok "$FLOW" check $TEMP_DIR


## Error tests

echo "err-infer-0.js"
assert_ok "$FLOW" suggest --strip-root --quiet err-infer-0.js

echo "err-infer-0.js (--fail-on-tc-errors)"
assert_errors "$FLOW" suggest --strip-root --quiet --fail-on-tc-errors err-infer-0.js 2>&1

echo "err-parse-0.js"
assert_errors "$FLOW" suggest --strip-root --quiet --quiet err-parse-0.js 2>&1
echo "err-parse-1.js"
assert_errors "$FLOW" suggest --strip-root --quiet --quiet err-parse-1.js 2>&1

echo "warn-empty-0.js"
assert_ok "$FLOW" suggest --strip-root --quiet warn-empty-0.js 2>&1

echo "func-6.js"
assert_ok "$FLOW" suggest --strip-root --quiet "func-6.js"

echo "warn-empty-0.js (--fail-on-suggest-warnings)"
assert_errors "$FLOW" suggest --strip-root --quiet --fail-on-suggest-warnings warn-empty-0.js 2>&1

echo "warn-empty-0.js (--fail-on-tc-errors)"
assert_ok "$FLOW" suggest --strip-root --quiet --fail-on-tc-errors warn-empty-0.js

echo "warn-func-poly-0.js"
assert_errors "$FLOW" suggest --strip-root --quiet --fail-on-suggest-warnings warn-func-poly-0.js 2>&1

echo "warn-anonymous-class.js"
assert_errors "$FLOW" suggest --strip-root --quiet --fail-on-suggest-warnings warn-anonymous-class.js 2>&1
