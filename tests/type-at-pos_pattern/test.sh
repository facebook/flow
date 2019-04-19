#!/bin/bash

# object.js
printf "object.js:11:3 = "
assert_ok "$FLOW" type-at-pos object.js 11 3 --strip-root --pretty
printf "object.js:12:3 = "
assert_ok "$FLOW" type-at-pos object.js 12 3 --strip-root --pretty
printf "object.js:13:3 = "
assert_ok "$FLOW" type-at-pos object.js 13 3 --strip-root --pretty
printf "object.js:14:3 = "
assert_ok "$FLOW" type-at-pos object.js 14 3 --strip-root --pretty
printf "object.js:15:4 = "
assert_ok "$FLOW" type-at-pos object.js 15 4 --strip-root --pretty
