#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" get-def --strip-root --json ignore/test.js 3 2
assert_ok "$FLOW" type-at-pos --strip-root --json ignore/test.js 3 2
assert_ok "$FLOW" get-def --strip-root --json no_flow/test.js 3 2
assert_ok "$FLOW" type-at-pos --strip-root --json no_flow/test.js 3 2
