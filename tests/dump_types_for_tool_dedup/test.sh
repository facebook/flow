#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# `--dedup` wraps the output as {"types": [...], "$defs": {...}} and replaces
# repeated subtrees over the threshold with {"$ref": id}. Threshold 0 promotes
# essentially everything, which keeps the shared structure visible in a file
# this small.
assert_ok "$FLOW" dump-types --for-tool --dedup --strip-root --json --pretty test.js
assert_ok "$FLOW" dump-types --for-tool --dedup 0 --strip-root --json --pretty test.js
