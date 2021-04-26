#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

echo "============================================"
echo "No warnings:"
echo "============================================"
assert_errors "$FLOW" check --no-flowlib

echo ""
echo "============================================"
echo "With warnings:"
echo "============================================"
assert_errors "$FLOW" check --no-flowlib --include-warnings
