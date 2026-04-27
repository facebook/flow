#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Normalize numeric Poly type ids (e.g. \"id\":\"14346\") to a placeholder
# because they come from a global counter whose value depends on the order
# of all type allocations across the typing pipeline. The OCaml and Rust
# port implementations differ in cumulative allocation count, so the absolute
# id number is not portable. The structural type info is preserved.
assert_ok "$FLOW" dump-types --for-tool --strip-root --json --pretty test.js \
  | sed -E 's/\\"id\\":\\"[0-9]+\\"/\\"id\\":\\"<ID>\\"/g'
