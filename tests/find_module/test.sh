#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

assert_ok "$FLOW" find-module --strip-root --json ./req test.js
assert_ok "$FLOW" get-def --strip-root --json test.js 3 2
assert_ok "$FLOW" get-def --strip-root --json test.js 2 10
assert_ok "$FLOW" get-def --strip-root --json test.js 5 18
