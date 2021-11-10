#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" dump-types --strip-root --json --pretty test.js

echo "=== predicates.js ==="
assert_ok "$FLOW" dump-types --strip-root --json --pretty predicates.js

echo "=== type-destructors.js ==="
assert_ok "$FLOW" dump-types --strip-root type-destructors.js | grep '^type-destructors.js:7'

echo "=== type-destructors.js (--evaluate-type-destructors) ==="
assert_ok "$FLOW" dump-types --strip-root --evaluate-type-destructors type-destructors.js | grep '^type-destructors.js:7'
