#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "Don't crash on MergedT:"
assert_ok "$FLOW" find-refs --global --json --pretty --strip-root test.js 7 3
