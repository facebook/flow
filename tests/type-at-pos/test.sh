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

# implements.js
printf "implements.js:7:8 = "
assert_ok "$FLOW" type-at-pos implements.js 4 23 --strip-root --pretty

# module_export.js
printf "module_export.js:3:24 = "
assert_ok "$FLOW" type-at-pos module_export.js 3 24 --strip-root --pretty
printf "module_export.js:5:25 = "
assert_ok "$FLOW" type-at-pos module_export.js 5 25 --strip-root --pretty

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

printf "predicates.js - y (refined obj): "
assert_ok "$FLOW" type-at-pos predicates.js 8 5 --strip-root --pretty
printf "predicates.js - if (y.FOO) obj: "
assert_ok "$FLOW" type-at-pos predicates.js 9 5 --strip-root --pretty
printf "predicates.js - if (y.FOO) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 9 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == '') obj: "
assert_ok "$FLOW" type-at-pos predicates.js 10 5 --strip-root --pretty
printf "predicates.js - if (y.FOO == '') prop: "
assert_ok "$FLOW" type-at-pos predicates.js 10 8 --strip-root --pretty
printf "predicates.js - if (y.FOO === '') obj: "
assert_ok "$FLOW" type-at-pos predicates.js 11 5 --strip-root --pretty
printf "predicates.js - if (y.FOO === '') prop: "
assert_ok "$FLOW" type-at-pos predicates.js 11 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == null) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 12 8 --strip-root --pretty
printf "predicates.js - if (y.FOO == undefined) prop: "
assert_ok "$FLOW" type-at-pos predicates.js 13 8 --strip-root --pretty
printf "predicates.js - if (Array.isArray(y.FOO)): "
assert_ok "$FLOW" type-at-pos predicates.js 14 22 --strip-root --pretty

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

printf "object.js:8:16 = "
assert_ok "$FLOW" type-at-pos object.js 8 16 --strip-root --pretty
printf "object.js:14:7 = "
assert_ok "$FLOW" type-at-pos object.js 14 7 --strip-root --pretty
printf "object.js:15:3 = "
assert_ok "$FLOW" type-at-pos object.js 15 3 --strip-root --pretty
printf "object.js:16:3 = "
assert_ok "$FLOW" type-at-pos object.js 16 3 --strip-root --pretty
printf "object.js:19:7 = "
assert_ok "$FLOW" type-at-pos object.js 19 7 --strip-root --pretty
printf "object.js:20:7 = "
assert_ok "$FLOW" type-at-pos object.js 20 7 --strip-root --pretty
printf "object.js:26:4 = "
assert_ok "$FLOW" type-at-pos object.js 26 4 --strip-root --pretty
printf "object.js:32:4 = "
assert_ok "$FLOW" type-at-pos object.js 32 4 --strip-root --pretty
printf "object.js:32:6 = "
assert_ok "$FLOW" type-at-pos object.js 32 6 --strip-root --pretty
printf "object.js:33:21 = "
assert_ok "$FLOW" type-at-pos object.js 33 21 --strip-root --pretty
printf "object.js:33:24 = "
assert_ok "$FLOW" type-at-pos object.js 33 24 --strip-root --pretty
printf "object.js:38:6 = "
assert_ok "$FLOW" type-at-pos object.js 38 6 --strip-root --pretty
printf "object.js:42:17 = "
assert_ok "$FLOW" type-at-pos object.js 42 17 --strip-root --pretty
printf "object.js:46:3 = "
assert_ok "$FLOW" type-at-pos object.js 46 3 --strip-root --pretty
printf "object.js:47:3 = "
assert_ok "$FLOW" type-at-pos object.js 47 3 --strip-root --pretty
printf "object.js:52:5 = "
assert_ok "$FLOW" type-at-pos object.js 52 5 --strip-root --pretty
printf "object.js:55:5 = "
assert_ok "$FLOW" type-at-pos object.js 55 5 --strip-root --pretty

printf "object.js:58:14 = "
assert_ok "$FLOW" type-at-pos object.js 58 14 --strip-root --pretty
printf "object.js:59:6 = "
assert_ok "$FLOW" type-at-pos object.js 59 6 --strip-root --pretty
printf "object.js:60:6 = "
assert_ok "$FLOW" type-at-pos object.js 60 6 --strip-root --pretty
printf "object.js:62:15 = "
assert_ok "$FLOW" type-at-pos object.js 62 15 --strip-root --pretty
printf "object.js:63:10 = "
assert_ok "$FLOW" type-at-pos object.js 63 10 --strip-root --pretty
printf "object.js:64:10 = "
assert_ok "$FLOW" type-at-pos object.js 64 10 --strip-root --pretty

printf "object.js:66:7 = "
assert_ok "$FLOW" type-at-pos object.js 66 7 --strip-root --pretty
printf "object.js:66:11 = "
assert_ok "$FLOW" type-at-pos object.js 66 11 --strip-root --pretty
# TODO PV fix expected result
printf "object.js:68:7 = "
assert_ok "$FLOW" type-at-pos object.js 68 7 --strip-root --pretty
printf "object.js:69:7 = "
assert_ok "$FLOW" type-at-pos object.js 69 7 --strip-root --pretty
printf "object.js:69:11 = "
assert_ok "$FLOW" type-at-pos object.js 69 11 --strip-root --pretty
printf "object.js:71:5 = "
assert_ok "$FLOW" type-at-pos object.js 71 5 --strip-root --pretty
printf "object.js:72:14 = "
assert_ok "$FLOW" type-at-pos object.js 72 14 --strip-root --pretty

printf "object.js:79:7 = "
assert_ok "$FLOW" type-at-pos object.js 79 7 --strip-root --pretty
# TODO PV fix expected result
printf "object.js:79:31 = "
assert_ok "$FLOW" type-at-pos object.js 79 31 --strip-root --pretty
printf "object.js:80:3 = "
assert_ok "$FLOW" type-at-pos object.js 80 3 --strip-root --pretty
printf "object.js:80:8 = "
assert_ok "$FLOW" type-at-pos object.js 80 8 --strip-root --pretty
printf "object.js:81:8 = "
assert_ok "$FLOW" type-at-pos object.js 81 8 --strip-root --pretty

# TODO PV fix expected result
printf "object.js:84:8 = "
assert_ok "$FLOW" type-at-pos object.js 84 8 --strip-root --pretty

printf "object.js:89:8 = "
assert_ok "$FLOW" type-at-pos object.js 89 8 --strip-root --pretty
# TODO PV fix expected result
printf "object.js:90:10 = "
assert_ok "$FLOW" type-at-pos object.js 90 10 --strip-root --pretty
