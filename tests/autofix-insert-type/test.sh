#!/bin/bash

## Correct tests

TEMP_DIR=tmp
mkdir $TEMP_DIR

do_file() {
  FILE="$1"; shift;
  echo "$FILE" "$@"
  cp "$FILE" "$TEMP_DIR/$FILE"
  assert_ok "$FLOW" autofix insert-type --strip-root --quiet --in-place --path="$TEMP_DIR/out.js" \
    "$TEMP_DIR/$FILE" "$@"
  cat "$TEMP_DIR/out.js"
  rm "$TEMP_DIR/$FILE"
  assert_ok "$FLOW" force-recheck "$TEMP_DIR/out.js"
  assert_ok "$FLOW" status
  rm "$TEMP_DIR/out.js"
}

do_file "array.js" 3 15 --strategy=specialize
do_file "arrow-0.js" 3 8
do_file "arrow-0.js" 3 13
do_file "arrow-0.js" 3 14
do_file "arrow-1.js" 3 16
do_file "arrow-2.js" 3 5
do_file "class-0.js" 4 3
do_file "class-0.js" 4 4
do_file "class-0.js" 4 3 4 4
do_file "class-1.js" 5 6
do_file "class-2.js" 4 6
do_file "class-3.js" 9 15
do_file "comments-0.js" 4 33
do_file "dictionary.js" 12 12
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
do_file "poly-0.js" 3 22 --strategy=specialize
do_file "poly-0.js" 3 15
do_file "react-0.js" 6 21
do_file "string-literal.js" 11 14
do_file "type-utils.js" 6 3
do_file "union-0.js" 3 15
do_file "replacement-array.js" 5 15 --strategy=generalize
do_file "replacement-arrow.js" 3 13
do_file "replacement-class.js" 5 7
do_file "replacement-dictionary.js" 3 3 --strategy=generalize
do_file "replacement-function.js" 7 47
do_file "replacement-object.js" 2 16

do_file "func-0.js" 4 14 4 15
do_file "func-0.js" 4 10 4 15
do_file "arrow-0.js" 3 11 3 23
do_file "arrow-0.js" 4 1 4 5
do_file "arrow-0.js" 3 7 3 8

do_file "alias.js" 5 24
do_file "alias.js" 5 24 --expand-type-aliases
do_file "alias.js" 6 10 6 11
do_file "alias.js" 9 1 9 5 --expand-type-aliases

do_file "alias-0.js" 9 24
do_file "alias-0.js" 9 24 --expand-type-aliases
do_file "alias-0.js" 10 10 10 11
do_file "alias-0.js" 13 1 13 32 --expand-type-aliases

# Test pointing to identifiers
do_file "replacement-object.js" 2 5 --strategy=generalize
do_file "replacement-object.js" 2 6 --strategy=specialize
do_file "replacement-object.js" 2 7 --strategy=temporary
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet \
    "replacement-object.js" 2 7 --strategy=fail
do_file "replacement-object.js" 2 8 --strategy=fixme
do_file "replacement-object.js" 2 5 2 8 --strategy=generalize
do_file "func-2.js" 3 14
do_file "func-2.js" 3 17 3 18
do_file "func-2.js" 4 16
do_file "func-2.js" 4 19 4 20
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet --strict-location "func-2.js" 3 14
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet --strict-location "func-2.js" 3 17 3 18
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet --strict-location "func-2.js" 4 16
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet --strict-location "func-2.js" 4 19 4 20


assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet "object-0.js" 4 4
assert_exit 110 "$FLOW" autofix insert-type --strip-root --quiet "poly-0.js" 3 21

# Test File IO
echo "insert-type array.js 3:15"
cp "array.js" "$TEMP_DIR/array.js"
assert_ok "$FLOW" autofix insert-type --strip-root --quiet --in-place \
  --strategy=specialize "$TEMP_DIR/array.js" 3 15
echo "cat array.js 3:15"
cat "$TEMP_DIR/array.js"

echo "insert-type arrow-0.js 3:11-3:23"
cp "arrow-0.js" "$TEMP_DIR/arrow-0.js"
assert_ok "$FLOW" autofix insert-type --strip-root --quiet \
  --in-place --path "$TEMP_DIR/arrow-0-copy.js" "$TEMP_DIR/arrow-0.js" 3 11 3 23
echo "cat arrow-0-copy.js 3:11-3:23"
cat "$TEMP_DIR/arrow-0-copy.js"
echo "cat arrow-0.js 3:11-3:23"
cat "$TEMP_DIR/arrow-0.js"
