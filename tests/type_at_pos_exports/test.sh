#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "a.js:1:16 = "
assert_ok "$FLOW" type-at-pos a.js 1 16 --strip-root --pretty

printf "b.js:3:16 = "
assert_ok "$FLOW" type-at-pos b.js 3 16 --strip-root --pretty

printf "c.js:1:16 = "
assert_ok "$FLOW" type-at-pos c.js 1 16 --strip-root --pretty
