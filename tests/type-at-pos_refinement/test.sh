#!/bin/bash

# predicates.js
# printf "predicates.js:4:12 (null) = "
# assert_ok "$FLOW" type-at-pos predicates.js 4 12 --strip-root --pretty
printf "predicates.js - undefined: "
assert_ok "$FLOW" type-at-pos predicates.js 5 12 --strip-root --pretty
printf "predicates.js - Array: "
assert_ok "$FLOW" type-at-pos predicates.js 6 6 --strip-root --pretty
printf "predicates.js - isArray: "
assert_ok "$FLOW" type-at-pos predicates.js 6 15 --strip-root --pretty
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

# refinement.js
printf "refinement.js:7:25 = "
assert_ok "$FLOW" type-at-pos refinement.js 7 25 --strip-root --pretty
printf "refinement.js:8:25 = "
assert_ok "$FLOW" type-at-pos refinement.js 8 25 --strip-root --pretty
