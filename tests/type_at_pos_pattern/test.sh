#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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

# A destructured binding is framed by the declaration it resolves to, so `b: b1`
# reports the bound name rather than the property name. Queried in friendly mode,
# since the `--pretty` payload above is a bare type either way.
printf "object.js:11:3 (framed) = "
assert_ok "$FLOW" type-at-pos object.js 11 3 --strip-root
printf "object.js:12:6 (framed) = "
assert_ok "$FLOW" type-at-pos object.js 12 6 --strip-root
