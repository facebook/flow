#!/bin/bash

# interface.js
printf "interface.js:3:12 = "
assert_ok "$FLOW" type-at-pos interface.js 3 12 --strip-root --pretty
printf "interface.js:9:15 = "
assert_ok "$FLOW" type-at-pos interface.js 9 15 --strip-root --pretty
printf "interface.js:9:19 = "
assert_ok "$FLOW" type-at-pos interface.js 9 19 --strip-root --pretty
# # TODO: report specialized type
# printf "interface.js:10:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 10 6 --strip-root --pretty
# printf "interface.js:11:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 11 6 --strip-root --pretty
# printf "interface.js:13:6 = "
# assert_ok "$FLOW" type-at-pos interface.js 13 6 --strip-root --pretty
printf "interface.js:17:7 = "
assert_ok "$FLOW" type-at-pos interface.js 17 7 --strip-root --pretty
printf "interface.js:18:7 = "
assert_ok "$FLOW" type-at-pos interface.js 18 7 --strip-root --pretty
