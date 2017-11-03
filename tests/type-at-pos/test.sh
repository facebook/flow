#!/bin/bash
. ../assert.sh
FLOW=$1

assert_ok \
  "$FLOW" type-at-pos test.js 5 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos test.js 8 7 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos test.js 10 7 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos test.js 12 7 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos test.js 14 7 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 5 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 10 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 14 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 18 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 22 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos generics.js 26 1 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 4 10 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 7 2 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 7 4 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 10 11 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 10 14 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos optional.js 14 10 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos destructuring.js 3 6 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos destructuring.js 17 13 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos react.js 2 7 --strip-root --pretty
assert_ok \
  "$FLOW" type-at-pos trycatch.js 5 10 --strip-root --pretty

# printf "function_expression.js:4:8 = "
# assert_ok \
#   "$FLOW" type-at-pos function_expressions.js 4 12 --strip-root --pretty
printf "function_expression.js:9:5 = "
assert_ok \
  "$FLOW" type-at-pos function_expressions.js 9 5 --strip-root --pretty
printf "function_expression.js:9:7 = "
assert_ok \
  "$FLOW" type-at-pos function_expressions.js 9 7 --strip-root --pretty
printf "function_expression.js:13:5 = "
assert_ok \
  "$FLOW" type-at-pos function_expressions.js 13 5 --strip-root --pretty
printf "function_expression.js:17:3 = "
assert_ok \
  "$FLOW" type-at-pos function_expressions.js 17 3 --strip-root --pretty
printf "function_expression.js:21:3 = "
assert_ok \
  "$FLOW" type-at-pos function_expressions.js 21 3 --strip-root --pretty
printf "predicates.js - null: "
assert_ok \
  "$FLOW" type-at-pos predicates.js 4 12 --strip-root --pretty
printf "predicates.js - undefined: "
assert_ok \
  "$FLOW" type-at-pos predicates.js 5 12 --strip-root --pretty
printf "predicates.js - Array: "
assert_ok \
  "$FLOW" type-at-pos predicates.js 6 6 --strip-root --pretty
printf "predicates.js - isArray: "
assert_ok \
  "$FLOW" type-at-pos predicates.js 6 15 --strip-root --pretty
printf "templates.js:2:7 = "
assert_ok \
  "$FLOW" type-at-pos templates.js 2 7 --strip-root --pretty
printf "object_special_cases.js:6:32 = "
assert_ok \
  "$FLOW" type-at-pos object_special_cases.js 6 32 --strip-root --pretty
printf "mixed.js:18:17 = "
assert_ok \
  "$FLOW" type-at-pos mixed.js 18 17 --strip-root --pretty
printf "array.js:6:15 = "
assert_ok \
  "$FLOW" type-at-pos array.js 6 15 --strip-root --pretty
printf "array.js:10:15 = "
assert_ok \
  "$FLOW" type-at-pos array.js 10 15 --strip-root --pretty
