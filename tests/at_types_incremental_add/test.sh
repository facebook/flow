#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Clean up the @types/foo dir on exit so the test directory matches its
# initial state regardless of which step we exited at.
trap 'rm -rf node_modules/@types' EXIT

printf "======Step 1: foo untyped, no @types/foo => val1 is any, no errors======\n"
assert_ok "$FLOW" status --no-auto-start .

printf "\n\n======Step 2: add @types/foo => @types resolution wins, val1 is number======\n"
mkdir -p node_modules/@types/foo
cp tmp/at_types_foo_index.d.ts node_modules/@types/foo/index.d.ts
assert_ok "$FLOW" force-recheck --no-auto-start node_modules/@types/foo/index.d.ts
assert_errors "$FLOW" status --no-auto-start .

printf "\n\n======Step 3: remove @types/foo => fall back to untyped foo, no errors======\n"
rm -rf node_modules/@types
assert_ok "$FLOW" force-recheck --no-auto-start node_modules/@types/foo/index.d.ts
assert_ok "$FLOW" status --no-auto-start .
