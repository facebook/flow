#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

printf "======Step 1: foo is untyped, resolves to @types/foo (val1: number)======\n"
assert_errors "$FLOW" status --no-auto-start .

printf '\n\n======Step 2: foo gains @flow `val1 as string`, should take priority over @types======\n'
cp node_modules/foo/index.js node_modules/foo/index.js.orig
trap 'mv node_modules/foo/index.js.orig node_modules/foo/index.js' EXIT
cp tmp/index.js node_modules/foo/index.js
assert_ok "$FLOW" force-recheck --no-auto-start node_modules/foo/index.js
assert_errors "$FLOW" status --no-auto-start .
