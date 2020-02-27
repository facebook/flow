#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "simple ES6 exports/imports:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root es6-1.js 6 17
