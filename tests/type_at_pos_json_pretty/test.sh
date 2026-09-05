#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This is the one place that pins the JSON envelope of `type-at-pos`
# (`--pretty` implies `--json`). Everywhere else type-at-pos is tested in
# friendly/hover mode. Keep this small: one case per envelope shape.

# Basic envelope: bare type string + loc object
printf "basic.js:3:7 = "
assert_ok "$FLOW" type-at-pos basic.js 3 7 --strip-root --pretty
printf "basic.js:4:7 = "
assert_ok "$FLOW" type-at-pos basic.js 4 7 --strip-root --pretty
printf "basic.js:6:1 = "
assert_ok "$FLOW" type-at-pos basic.js 6 1 --strip-root --pretty

# Null envelope: position past EOF has no type
printf "basic.js:8:5 = "
assert_ok "$FLOW" type-at-pos basic.js 8 5 --strip-root --pretty

# Documentation field
printf "docs.js:6:7 = "
assert_ok "$FLOW" type-at-pos docs.js 6 7 --strip-root --pretty
printf "docs.js:8:1 = "
assert_ok "$FLOW" type-at-pos docs.js 8 1 --strip-root --pretty

# Expanded output: structural dump alongside the type string
printf "spread.js:4:7 = "
assert_ok "$FLOW" type-at-pos spread.js 4 7 --strip-root --pretty --expand-json-output
printf "main.js:5:10 = "
assert_ok "$FLOW" type-at-pos main.js 5 10 --strip-root --pretty --expand-json-output
