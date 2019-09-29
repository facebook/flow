#!/bin/bash

# typed.js
printf "typed.js:2:7 = "
assert_ok "$FLOW" type-at-pos typed.js 2 7 --strip-root --pretty

printf "typed.js:4:5 = "
assert_ok "$FLOW" type-at-pos typed.js 4 5 --strip-root --pretty

printf "typed.js:5:5 = "
assert_ok "$FLOW" type-at-pos typed.js 5 5 --strip-root --pretty

printf "typed.js:5:9 = "
assert_ok "$FLOW" type-at-pos typed.js 5 9 --strip-root --pretty

printf "typed.js:6:5 = "
assert_ok "$FLOW" type-at-pos typed.js 6 5 --strip-root --pretty

printf "typed.js:6:9 = "
assert_ok "$FLOW" type-at-pos typed.js 6 9 --strip-root --pretty

printf "typed.js:8:5 = "
assert_ok "$FLOW" type-at-pos typed.js 8 5 --strip-root --pretty

printf "typed.js:8:9 = "
assert_ok "$FLOW" type-at-pos typed.js 8 9 --strip-root --pretty

printf "typed.js:9:5 = "
assert_ok "$FLOW" type-at-pos typed.js 9 5 --strip-root --pretty

printf "typed.js:9:9 = "
assert_ok "$FLOW" type-at-pos typed.js 9 9 --strip-root --pretty

printf "typed.js:9:12 = "
assert_ok "$FLOW" type-at-pos typed.js 9 12 --strip-root --pretty

printf "typed.js:10:5 = "
assert_ok "$FLOW" type-at-pos typed.js 10 5 --strip-root --pretty

printf "typed.js:10:9 = "
assert_ok "$FLOW" type-at-pos typed.js 10 9 --strip-root --pretty

printf "typed.js:12:5 = "
assert_ok "$FLOW" type-at-pos typed.js 12 5 --strip-root --pretty

printf "typed.js:12:9 = "
assert_ok "$FLOW" type-at-pos typed.js 12 9 --strip-root --pretty

printf "typed.js:12:17 = "
assert_ok "$FLOW" type-at-pos typed.js 12 17 --strip-root --pretty

printf "typed.js:13:5 = "
assert_ok "$FLOW" type-at-pos typed.js 13 5 --strip-root --pretty

printf "typed.js:13:9 = "
assert_ok "$FLOW" type-at-pos typed.js 13 9 --strip-root --pretty

printf "typed.js:15:5 = "
assert_ok "$FLOW" type-at-pos typed.js 15 5 --strip-root --pretty

printf "typed.js:15:27 = "
assert_ok "$FLOW" type-at-pos typed.js 15 27 --strip-root --pretty

printf "typed.js:16:5 = "
assert_ok "$FLOW" type-at-pos typed.js 16 5 --strip-root --pretty

printf "typed.js:18:5 = "
assert_ok "$FLOW" type-at-pos typed.js 18 5 --strip-root --pretty

printf "typed.js:20:5 = "
assert_ok "$FLOW" type-at-pos typed.js 20 5 --strip-root --pretty

printf "typed.js:21:5 = "
assert_ok "$FLOW" type-at-pos typed.js 21 5 --strip-root --pretty

printf "typed.js:23:5 = "
assert_ok "$FLOW" type-at-pos typed.js 23 5 --strip-root --pretty

printf "typed.js:24:5 = "
assert_ok "$FLOW" type-at-pos typed.js 24 5 --strip-root --pretty

printf "typed.js:27:5 = "
assert_ok "$FLOW" type-at-pos typed.js 27 5 --strip-root --pretty

printf "typed.js:28:5 = "
assert_ok "$FLOW" type-at-pos typed.js 28 5 --strip-root --pretty
