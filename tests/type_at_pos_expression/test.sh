#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# dictionary
printf "dictionary.js:3:7 = "
assert_ok "$FLOW" type-at-pos dictionary.js 3 7 --strip-root --pretty
printf "dictionary.js:19:7 = "
assert_ok "$FLOW" type-at-pos dictionary.js 19 7 --strip-root --pretty

# facebookism.js
printf "facebookism.js:3:8 = "
assert_ok "$FLOW" type-at-pos facebookism.js 3 8 --strip-root --pretty
# TODO `require`
# printf "facebookism.js:3:14 = "
# assert_ok "$FLOW" type-at-pos facebookism.js 3 14 --strip-root --pretty

# jsx.js
printf "jsx.js:7:12 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 12 --strip-root --pretty
printf "jsx.js:7:16 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 16 --strip-root --pretty
printf "jsx.js:7:23 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 23 --strip-root --pretty
printf "jsx.js:7:29 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 29 --strip-root --pretty
printf "jsx.js:7:35 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 35 --strip-root --pretty
printf "jsx.js:7:55 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 55 --strip-root --pretty
printf "jsx.js:7:61 = "
assert_ok "$FLOW" type-at-pos jsx.js 7 61 --strip-root --pretty
printf "jsx.js:8:17 = "
assert_ok "$FLOW" type-at-pos jsx.js 8 17 --strip-root --pretty
printf "jsx.js:8:23 = "
assert_ok "$FLOW" type-at-pos jsx.js 8 23 --strip-root --pretty
printf "jsx.js:8:30 = "
assert_ok "$FLOW" type-at-pos jsx.js 8 30 --strip-root --pretty
printf "jsx.js:11:17 = "
assert_ok "$FLOW" type-at-pos jsx.js 11 17 --strip-root --pretty
printf "jsx.js:18:12 = "
assert_ok "$FLOW" type-at-pos jsx.js 18 12 --strip-root --pretty
printf "jsx.js:18:14 = "
assert_ok "$FLOW" type-at-pos jsx.js 18 14 --strip-root --pretty
printf "jsx.js:18:12 = "
assert_ok "$FLOW" type-at-pos jsx.js 18 22 --strip-root --pretty
printf "jsx.js:18:24 = "
assert_ok "$FLOW" type-at-pos jsx.js 18 24 --strip-root --pretty

# literals.js
printf "literals.js:6:7 = "
assert_ok "$FLOW" type-at-pos literals.js 6 7 --strip-root --pretty
printf "literals.js:7:7 = "
assert_ok "$FLOW" type-at-pos literals.js 7 7 --strip-root --pretty
printf "literals.js:8:7 = "
assert_ok "$FLOW" type-at-pos literals.js 8 7 --strip-root --pretty
printf "literals.js:9:7 = "
assert_ok "$FLOW" type-at-pos literals.js 9 7 --strip-root --pretty
printf "literals.js:10:7 = "
assert_ok "$FLOW" type-at-pos literals.js 10 7 --strip-root --pretty
printf "literals.js:13:6 = "
assert_ok "$FLOW" type-at-pos literals.js 13 6 --strip-root --pretty
printf "literals.js:14:6 = "
assert_ok "$FLOW" type-at-pos literals.js 14 6 --strip-root --pretty
printf "literals.js:15:6 = "
assert_ok "$FLOW" type-at-pos literals.js 15 6 --strip-root --pretty
printf "literals.js:16:6 = "
assert_ok "$FLOW" type-at-pos literals.js 16 6 --strip-root --pretty
printf "literals.js:17:6 = "
assert_ok "$FLOW" type-at-pos literals.js 17 6 --strip-root --pretty
printf "literals.js:19:7 = "
assert_ok "$FLOW" type-at-pos literals.js 19 7 --strip-root --pretty
printf "literals.js:21:7 = "
assert_ok "$FLOW" type-at-pos literals.js 21 7 --strip-root --pretty
printf "literals.js:22:7 = "
assert_ok "$FLOW" type-at-pos literals.js 22 7 --strip-root --pretty
printf "literals.js:24:7 = "
assert_ok "$FLOW" type-at-pos literals.js 24 7 --strip-root --pretty
printf "literals.js:28:7 = "
assert_ok "$FLOW" type-at-pos literals.js 28 7 --strip-root --pretty
printf "literals.js:31:7 = "
assert_ok "$FLOW" type-at-pos literals.js 31 7 --strip-root --pretty
printf "literals.js:32:7 = "
assert_ok "$FLOW" type-at-pos literals.js 32 7 --strip-root --pretty

# new-array.js
printf "new-array.js:3:15 = "
assert_ok "$FLOW" type-at-pos new-array.js 3 15 --strip-root --pretty

# object-resolution.js
printf "object-resolution.js:5:2 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 5 2 --strip-root --pretty
printf "object-resolution.js:10:2 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 10 2 --strip-root --pretty
printf "object-resolution.js:13:5 = "
assert_ok "$FLOW" type-at-pos object-resolution.js 13 5 --strip-root --pretty

# object.js
printf "object.js:3:15 = "
assert_ok "$FLOW" type-at-pos object.js 3 15 --strip-root --pretty
printf "object.js:3:19 = "
assert_ok "$FLOW" type-at-pos object.js 3 19 --strip-root --pretty
printf "object.js:3:24 = "
assert_ok "$FLOW" type-at-pos object.js 3 24 --strip-root --pretty
printf "object.js:3:29 = "
assert_ok "$FLOW" type-at-pos object.js 3 29 --strip-root --pretty
printf "object.js:3:40 = "
assert_ok "$FLOW" type-at-pos object.js 3 40 --strip-root --pretty
printf "object.js:6:5 = "
assert_ok "$FLOW" type-at-pos object.js 6 5 --strip-root --pretty
printf "object.js:6:7 = " # TODO can we do better with duplication?
assert_ok "$FLOW" type-at-pos object.js 6 7 --strip-root --pretty
printf "object.js:7:10 = "
assert_ok "$FLOW" type-at-pos object.js 7 10 --strip-root --pretty
printf "object.js:7:12 = "
assert_ok "$FLOW" type-at-pos object.js 7 12 --strip-root --pretty
printf "object.js:8:14 = "
assert_ok "$FLOW" type-at-pos object.js 8 14 --strip-root --pretty
printf "object.js:8:16 = "
assert_ok "$FLOW" type-at-pos object.js 8 16 --strip-root --pretty
printf "object.js:9:18 = "
assert_ok "$FLOW" type-at-pos object.js 9 18 --strip-root --pretty
printf "object.js:9:34 = "
assert_ok "$FLOW" type-at-pos object.js 9 34 --strip-root --pretty
printf "object.js:15:3 = "
assert_ok "$FLOW" type-at-pos object.js 15 3 --strip-root --pretty
printf "object.js:16:3 = "
assert_ok "$FLOW" type-at-pos object.js 16 3 --strip-root --pretty
printf "object.js:19:3 = "
assert_ok "$FLOW" type-at-pos object.js 19 3 --strip-root --pretty
printf "object.js:19:7 = "
assert_ok "$FLOW" type-at-pos object.js 19 7 --strip-root --pretty
printf "object.js:20:7 = "
assert_ok "$FLOW" type-at-pos object.js 20 7 --strip-root --pretty
printf "object.js:21:7 = "
assert_ok "$FLOW" type-at-pos object.js 21 7 --strip-root --pretty
printf "object.js:22:7 = "
assert_ok "$FLOW" type-at-pos object.js 22 7 --strip-root --pretty
printf "object.js:35:1 = "
assert_ok "$FLOW" type-at-pos object.js 35 1 --strip-root --pretty

# optional_chaining.js
printf "optional_chaining.js:16:7 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 7 --strip-root --pretty
printf "optional_chaining.js:16:11 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 11 --strip-root --pretty
printf "optional_chaining.js:16:16 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 16 --strip-root --pretty
printf "optional_chaining.js:16:20 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 20 --strip-root --pretty
printf "optional_chaining.js:16:24 = "
assert_ok "$FLOW" type-at-pos optional_chaining.js 16 24 --strip-root --pretty

queries_in_file "type-at-pos" "identifier.js"
